{-# LANGUAGE
    TypeApplications
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , OverloadedStrings
  , DeriveGeneric
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
      - (TODO) if current time exceeds 10 mins more than an event's happening time, it should be dropped.
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

  - (draft) for reminding about maintenance events

    + a dedicated thread will try to grab maintenance from multiple sources
      (core logic are already implemented through Javran.WhaleChan.FromSource.*)
      and send parsed maintenance updates to this thread
      (in future we can make this dedicated thread stateful so it only sends update message
      when a difference is detected)
    + note that it's important that we use PRange for this purpose, rather than
      dealing with maintenance start and end time separately

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
    -- TODO: we only wait for 5s now for it's easier to debug
    -- threadDelay (oneSec * 5)

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
  ( Maybe (EventReminder, [String])
  , Maybe (EventReminder, [String])
  )

-- the ' version is actual resentation without newtype wrappers
type ReminderState = (ReminderDict, MaintenanceEventReminder)
type ReminderState' = (ReminderMap, MaintenanceEventReminder)
type ReminderM = WCM ReminderState
type ReminderM' = WCM ReminderState'

reminderThread :: WEnv -> IO ()
reminderThread wenv = do
    let cv :: forall a. ReminderM' a -> ReminderM a
        cv = coerce -- to avoid the noise introduced by newtype
        (_, TCommon{tcTelegram, tcReminder}) = wenv
    -- ref: https://stackoverflow.com/q/43835656/315302
    -- load tz info before starting the loop
    _tzPt <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/US/Pacific"
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/Asia/Tokyo"
    autoWCM @ReminderState "Reminder" "reminder.yaml" wenv $ \markStart' -> cv $ do
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
      Log.i "Reminder" ("MaintenanceInfo: " <> show mInfo)
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
            mValuePre <- gets (M.lookup tyRep . fst)
            let mValue = dropWhile isOutdatedEventReminder <$> mValuePre
            -- restock step
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
      unless (null displayList) $ do
        let txt = toStrict (TB.toLazyText md)
            md = foldMap pprERS displayList
            pprERS (ERS tp, eTimes) =
                "- Reminder: **" <> TB.fromString (show tyRep) <> "**\n" <>
                  foldMap pprTime eTimes
              where
                tyRep = typeRep tp
                pprTime eTime =
                    "    + " <> TB.fromString timeStr <> "\n"
                  where
                    timeStr = describeDuration (round (eTime `diffUTCTime` curTime) :: Int)
        void $ liftIO $ writeChan tcTelegram (TgRMTimer txt (Just Markdown))
