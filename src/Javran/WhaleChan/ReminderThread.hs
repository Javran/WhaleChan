{-# LANGUAGE
    TypeApplications
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , OverloadedStrings
  , DeriveGeneric
  , TupleSections
  , FlexibleContexts
  , MultiWayIf
  #-}
module Javran.WhaleChan.ReminderThread
 ( reminderThread
 ) where

import Control.Arrow
import Control.Concurrent
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Coerce
import Data.Default
import Data.Function
import Data.List
import Data.List.Ordered
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.Text.Lazy (toStrict)
import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Olson
import Data.Typeable
import GHC.Generics
import Web.Telegram.API.Bot

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB

import Javran.WhaleChan.Types
import Javran.WhaleChan.Base
import Javran.WhaleChan.ReminderSupply
import Javran.WhaleChan.Util
import qualified Javran.WhaleChan.Log as Log

{-
  ReminderThread design:

  - concepts:

    + a EventReminder contains a sorted list of pending reminders
      around a particular event.
    + several type implements ReminderSupply, which, when called with
      current time generates a corresponding EventReminder.
      (NOTE: this is a pure function, we'll worry about parsing maintenance times later)
    + these types are promoted data types, unified by a common kind: ReminderSupplier

  - we'll have a list of reminders implemented, visible to the reminder thread

  - this thread keeps track of a Map from TypeRep of ReminderSupplier
    to a List of EventReminder (sorted ascendingly by event happening time)

  - the reminder thread is a loop that:

    + wakes up at (roughly) the beginning of every minute
    + maintains the Map
      - if current time exceeds 10 mins more than an event's happening time, it should be dropped.
        (this will prevent old persisted states say few days ago or something from showing up)
      - calls all ReminderSupply to see if the Map needs to be updated
        (this is done by comparing events' happening time of new supply to existing ones,
        new supply gets added to the list only if it does not match any known event happening time)

        Note that it's important that we do this "re-supply" during this step on every loop,
        this allows same event to show different time reminders in the situations that we have overlap

        e.g. a daily practice might have two reminding events happening at the same time:
        one that happens right now, and another happens 12 hours after.
        in this case, the Map from daily practice to the list will contain two elements corresponding
        to these two event times.
      - now an extra traverse on reminders is performed to split the Map into those that will need to be reminded
        and remaining part of the Map becomes the new state.
    + if it's time to send a new reminder, the thread will construct the message and send it to telegramThread
      through channel

  - we'll basically reminder the following events:

      + "30 mins before [some event]"
      + "10 mins before [some event]"
      + "5 mins before [some event]"
      + "[some event] is happening"

    but this is by no meaning a limit, the definition of ReminderSupply
    is flexible to allow time much more longer - for example

  - for reminding about maintenance events

    + ExtInfoThread will try to grab maintenance from multiple sources
      and place the info mutiple sources has agreed upon (i.e. earliest start / end time)
      into ReminderThread's channel (a MVar for now)
    + note that it's important that we use PRange for this purpose, rather than
      dealing with maintenance start and end time separately
    + ReminderThread keeps its own pairs of EventReminder for both start and end time,
      during each loop, it looks at the latest MVar and update itself accordingly
 -}


{-
  wait and wake up at (roughly) begining of the next minute
  -- https://stackoverflow.com/a/8578237/315302
 -}
waitUntilStartOfNextMinute :: IO ()
waitUntilStartOfNextMinute = do
    t <- getCurrentTime
    -- compute millseconds since beginning of current minute
    let ms = round (fromIntegral oneSec * realToFrac @_ @Double (utctDayTime t)) `rem` oneMin
    -- wait to start of next minute
    threadDelay $ oneMin - ms

reminderSupplies :: [EReminderSupply]
reminderSupplies =
    [ ERS (Proxy :: Proxy 'PracticeReset)
    , ERS (Proxy :: Proxy 'DailyQuestReset)
    , ERS (Proxy :: Proxy 'WeeklyQuestReset)
    , ERS (Proxy :: Proxy 'MonthlyQuestReset)
    , ERS (Proxy :: Proxy 'QuarterlyQuestReset)
    , ERS (Proxy :: Proxy 'ExtraOperationReset)
    , ERS (Proxy :: Proxy 'SenkaAccounting)
    , ERS (Proxy :: Proxy 'QuestPointDeadline)
    ]

-- a hack to allow "encoding / decoding" of TypeRep through Show instance
-- for now it's a safe assumption that conversion through Show is consistent
reminders :: [(String, TypeRep)]
reminders = f <$> reminderSupplies
  where
    f (ERS ty) = (show tRep, tRep)
      where
        tRep = typeRep ty

strToReminderTypeRep :: String -> Parser TypeRep
strToReminderTypeRep raw = maybe mzero pure (lookup raw reminders)

checkEventReminder :: EventReminder -> Maybe String
checkEventReminder (EventReminder x xs)
  | null xs = Just "event reminder has empty due list"
  | last xs /= x = Just "event reminder last event not matching occur time"
  | and $ zipWith (<) xs (tail xs) = Nothing
  | otherwise = Just "event reminder not is strict ascending order."

-- TODO: use lens-datetime

{-
  note that [EventReminder] is sorted in time order,
  and is supposed to have no more than 2 items - as reminders are restocked at that
  exact moment, there should be one passing (0 seconds) and new one being added.
  we are still under the assumption that no more than 2 reminders (with beforhand reminders)
  will happen at the same time, which is quite safe considering the nature of this game
  (i.e. frequent events shouldn't be reminded too often (< 24 hours) and less frequent
  will have a relatively large interval between them, large enough that the overlapping
  of beforehand reminds are very unlikely.)
 -}
type ReminderMap = M.Map TypeRep [EventReminder]
newtype ReminderDict
  = RD {getRD :: ReminderMap }
  deriving (Eq, Generic)

instance Default ReminderDict

instance ToJSON ReminderDict where
  toJSON (RD d) = toJSON @[(String,[EventReminder])] (first show <$> M.toList d)

instance FromJSON ReminderDict where
  parseJSON o =
      RD . M.fromList <$>
        (parseJSON @[(String, [EventReminder])] o >>= mapM convert)
    where
      convert :: (String, [EventReminder]) -> Parser (TypeRep, [EventReminder])
      convert (r, er) = (,) <$> strToReminderTypeRep r <*> pure er

type MaintenanceEventReminder =
  ( Maybe (EventReminder, [String]) -- INVARIANT: we'll keep sources sorted by `sort`
  , Maybe (EventReminder, [String]) -- same INVARIANT
  )

-- the ' version is actual resentation without newtype wrappers
type ReminderState = (ReminderDict, MaintenanceEventReminder)
type ReminderState' = (ReminderMap, MaintenanceEventReminder)
type ReminderM = WCM ReminderState
type ReminderM' = WCM ReminderState'

-- Message representation for a reminder message.
type MessageRep =
  [] ( String -- Event description (e.g. "Daily Quest Reset")
     , [ ( UTCTime -- Event occur time
         , [String] -- sources, only used by maintenance time reminders
         )
       ]
     )

convertResult :: [(EReminderSupply, [UTCTime])] -> MessageRep
convertResult = fmap (conv *** fmap (,[]))
  where
    conv (ERS p) = eventDescription p

renderMessage :: UTCTime -> MessageRep -> Maybe T.Text
renderMessage curTime xs =
    toStrict . TB.toLazyText <$>
      case filter (not . null . snd) xs of
        [] -> Nothing
        [(eDesc, [eTimeSrc])] ->
          {-
            when a single line is good enough for displaying
            example:
            > [Reminder] {some event}: {relative time}
           -}
          Just $ tag <> " " <> TB.fromString eDesc <> ": " <> renderTimeSrc eTimeSrc
        [(eDesc, eTimeSrcs)] ->
          {-
            we still have one single event but multiple reminders
            > [Reminder] {some event}:
            > + {relative time}
            > + {relative time}
           -}
          Just $
            tag <> " " <> TB.fromString eDesc <> ":\n"
            <> foldMap (\p -> "+ " <> renderTimeSrc p <> "\n") eTimeSrcs
        ys ->
          {-
            most general case
            > [Reminder]
            > - {some event}: {relative time}
            > - {some other event}:
            >     + {relative time}
            >     + {relative time}
           -}
          Just $
            tag <> "\n" <> foldMap pprBlock ys
  where
    renderTimeSrc (eTime, srcs) =
        timePart <> if null srcs then "" else " " <> srcPart
      where
        timePart =
          TB.fromString $ describeDuration (round (eTime `diffUTCTime` curTime) :: Int)
        srcPart = "(source: " <> TB.fromString (intercalate ", " srcs) <> ")"

    tag = "〖Reminder〗" :: TB.Builder
    pprBlock (eDesc, [eTimeSrc]) =
        "- " <> TB.fromString eDesc <> ": " <> renderTimeSrc eTimeSrc <> "\n"
    pprBlock (eDesc, eTimeSrcs) =
        "- " <> TB.fromString eDesc <> ":\n"
        <> foldMap (\p -> "    + " <> renderTimeSrc p <> "\n") eTimeSrcs

reminderThread :: WEnv -> IO ()
reminderThread wenv = do
    let cv :: forall a. ReminderM' a -> ReminderM a
        cv = coerce -- to avoid the noise introduced by newtype
        (_, TCommon{tcTelegram, tcReminder}) = wenv
        tag = "Reminder"
    -- ref: https://stackoverflow.com/q/43835656/315302
    -- load tz info before starting the loop
    _tzPt <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/US/Pacific"
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/Asia/Tokyo"
    autoWCM @ReminderState tag "reminder.yaml" wenv $ \markStart' -> cv $ do
      let markStart = coerce markStart' :: ReminderM' (ReminderM' ())
      -- note that unlike other threads, this one begins by thread sleep
      -- the idea is to start working immediately after wake up
      -- so we get the most accurate timestamp to work with
      curTime <- liftIO (waitUntilStartOfNextMinute >> getCurrentTime)
      -- any event in future within 20 seconds is also included
      -- this assume that we can always process all reminders within 20 seconds
      -- which should be way more than enough.
      let tThres = addUTCTime 20 curTime
          {-
            we consider a ER oudated if eventOccurTime < current time - 10 mins,
            in that case the old ERs should be dropped
           -}
          isOutdatedEventReminder (EventReminder eTime _) =
              eTime < addUTCTime (fromIntegral @Int $ -60 * 10) curTime
      markEnd <- markStart
      mInfo <- liftIO $ do
        v <- takeMVar tcReminder
        putMVar tcReminder v
        pure v
      mer <- gets snd
      let (newMer, mMsgRep) = runWriter (updateMER curTime mer mInfo)
      modify (second (const newMer))
      when (mer /= newMer) $ do
        Log.i tag "MER updated:"
        Log.i tag $ "old: " <> show mer
        Log.i tag $ "new: " <> show newMer
        Log.i tag $ "minfo: " <> show mInfo
        let (lMer, rMer) = newMer
            check Nothing = pure ()
            check (Just (er,_)) = case checkEventReminder er of
              Nothing -> Log.i tag "er checking ok."
              Just errMsg -> do
                  Log.w tag $ "er checking failed: " <> errMsg
                  Log.w tag $ "the er is: " <> show er
        check lMer
        check rMer
      let collectResults :: Endo [(EReminderSupply, UTCTime)] -> [(EReminderSupply, [UTCTime])]
          collectResults xsPre = convert <$> ys
            where
              ersToTyRep (ERS tp) = typeRep tp
              xs = appEndo xsPre []
              ys = groupBy ((==) `on` ersToTyRep . fst) xs
              convert ts = (fst . head $ ts, snd <$> ts)
      -- scan through supplies and try restocking if an new event timestamp is found
      displayList <- collectResults <$>
        execWriterT (forM reminderSupplies (\e@(ERS tp) -> do
            let tyRep = typeRep tp
                -- always compute new supply (lazily)
                newSupply = renewSupply tp tzs curTime
                cmp = comparing eventOccurTime
                rmOutdated xs =
                  case dropWhile isOutdatedEventReminder xs of
                    [] -> Nothing
                    ys -> Just ys
            -- cleanup step
            modify (first $ M.update rmOutdated tyRep)

            -- restock step
            mValue <- gets (M.lookup tyRep . fst)
            case mValue of
              Nothing ->
                modify (first $ M.insert tyRep [newSupply])
              Just ers ->
                -- skip restocking if timestamp mismatches
                unless (hasBy cmp ers newSupply) $
                  modify (first $ M.adjust (insertSetBy cmp newSupply) tyRep)
            eventReminders <- fromMaybe [] <$> gets (M.lookup tyRep . fst)
            newEventReminders <-
              catMaybes <$> forM eventReminders (\(EventReminder eot erds) -> do
                -- if remindsDue contains anything, we should send current reminder
                let (remindsDue, erds') = span (< tThres) erds
                unless (null remindsDue) $ tell . Endo $ ([(e, eot)] ++)
                pure $ if null erds'
                    then Nothing
                    else Just (EventReminder eot erds')
              )
            let newVal =
                  if null newEventReminders
                    then Nothing
                    else Just newEventReminders
            modify (first $ M.update (const newVal) tyRep)
        ))
      markEnd
      case renderMessage curTime $ mMsgRep <> convertResult displayList of
        Nothing -> pure ()
        Just txt ->
          void $ liftIO $ writeChan tcTelegram (TgRMTimer txt (Just Markdown))

-- TODO: maybe we should start using dlist
updateMER :: forall m. MonadWriter MessageRep m
          => UTCTime
          -> MaintenanceEventReminder
          -> MaintenanceInfo
          -> m MaintenanceEventReminder
updateMER curTime curERPair mInfo = do
    let (lCurER, rCurER) = curERPair
        (lInfo, rInfo) = mInfo
    -- TODO: only print maintenance end when the time is after start
    (,) <$> (doUpdate lCurER lInfo >>= stepMER curTime "Maintenance Start")
        <*> (doUpdate rCurER rInfo >>= stepMER curTime "Maintenance End")
  where
    doUpdate :: Maybe (EventReminder, [String])
             -> Maybe (UTCTime, [String])
             -> m (Maybe (EventReminder, [String]))
    doUpdate curER info
      | isNothing info =
        {-
          keep whatever it is if the source cannot confirm that
          since we don't allow ER with empty due list, this should mean
          curER will also be Nothing in most of the cases
         -}
        pure curER
      | Just (newT, newSrcs) <- info
      = if newT < curTime
          then
            -- we are getting an outdated source, ignoring.
            pure curER
          else case curER of
            Nothing ->
              -- we are not holding any ER for now, time to create one.
              Just <$> createNewER newT newSrcs
            Just (er@(EventReminder eT _), srcs) ->
              if | eT /= newT ->
                  -- update on time, we should update as well
                  Just <$> createNewER newT newSrcs
                   -- time matches from now on
                 | newSrcs == srcs ->
                   -- source matches, nothing todo
                   -- note that by enforcing sources to be sorted,
                   -- the comparison down to just `==`
                   pure curER
                 | otherwise ->
                   -- we need a silent update to include new sources
                   pure $ Just (er, newSrcs)
        | otherwise = error "unreachable" -- we are just examining info
      where
        createNewER :: UTCTime -> [String] -> m (EventReminder, [String])
        createNewER eventTime srcs = do
            let preCurTime =
                  -- an extra time in addition to existing ones to make
                  -- sure a time update shows up once we know it.
                  addUTCTime (-20) curTime
                dayInSecs = 86400
                {-
                  we want:
                  - 6 hours, 2 hour, 1 hour, 30 min, 10 min, 5 min, happening
                  - maintenance time at every day (step: 1 day)
                 -}
                predefDueTimes =
                  reverse
                    (takeWhile (>curTime)
                      -- TODO: we'd better have some tests.
                      (tail $ iterate (addUTCTime (fInt $ -1 * dayInSecs)) eventTime))
                  <> (mkTime <$> [6*60, 2*60, 60, 30, 10, 5, 0])
                dueTimes = preCurTime `insertSet` predefDueTimes
            pure (EventReminder eventTime dueTimes, srcs)
          where
            fInt = fromIntegral @Int
            mkTime mins = addUTCTime (fInt $ -60 * mins) eventTime

-- the stage where maintenance ER is discharged to full MessageRep
-- TODO: drop outdated ERs like we do for regular ERs
stepMER :: forall m. MonadWriter MessageRep m
        => UTCTime
        -> String
        -> Maybe (EventReminder, [String])
        -> m (Maybe (EventReminder, [String]))
stepMER curTime desc mER = case mER of
    Nothing -> pure Nothing
    Just (EventReminder eT erds, srcs) -> do
      let (remindsDue, erds') = span (< tThres) erds
      unless (null remindsDue) $ tell [(desc, [(eT, srcs)])]
      pure $ if null erds'
               then Nothing
               else Just (EventReminder eT erds', srcs)
  where
    tThres = addUTCTime 20 curTime
