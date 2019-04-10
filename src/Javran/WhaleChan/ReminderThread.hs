{-# LANGUAGE
    TypeApplications
  , MultiWayIf
  , NamedFieldPuns
  , ScopedTypeVariables
  , OverloadedStrings
  , TupleSections
  , FlexibleContexts
  #-}
module Javran.WhaleChan.ReminderThread
 ( reminderThread
 ) where

import Control.Arrow
import Control.Concurrent
import Control.Monad
import Control.Monad.Logger
import Control.Monad.RWS
import Control.Monad.Writer
import Data.Coerce
import Data.Function
import Data.List
import Data.List.Ordered
import Data.Maybe
import Data.Ord
import Data.Time.Clock
import Data.Typeable
import Web.Telegram.API.Bot

import qualified Data.Map.Strict as M
import qualified Data.DList as DL

import Javran.WhaleChan.Types
import Javran.WhaleChan.Base
import Javran.WhaleChan.ReminderThread.EventReminder
import Javran.WhaleChan.ReminderThread.Message
import Javran.WhaleChan.ReminderThread.Types
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

convertResult :: [(EReminderSupply, [UTCTime])] -> MessageRep
convertResult = DL.fromList . fmap (conv *** fmap (,[]))
  where
    conv (ERS p) = eventDescription p

reminderThread :: WEnv -> IO ()
reminderThread wenv = do
    let cv :: forall a. ReminderM' a -> ReminderM a
        cv = coerce -- to avoid the noise introduced by newtype
        (_, TCommon{tcTelegram, tcReminder, tzTokyo}) = wenv
        tag = "Reminder"

    autoWCM @ReminderState tag "reminder.yaml" wenv $ \markStart' -> cv $ do
      let markStart = coerce markStart' :: ReminderM' (ReminderM' ())
          info = Log.i tag
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
          isOutdatedEventReminder er =
              eventOccurTime er < addUTCTime (fromIntegral @Int $ -60 * 10) curTime
      markEnd <- markStart
      mInfo <- liftIO $ do
        v <- takeMVar tcReminder
        putMVar tcReminder v
        pure v
      mer <- gets snd
      let (newMer, mMsgRep) = runWriter (updateMER curTime mer mInfo)
      modify (second (const newMer))
      when (mer /= newMer) $ do
        info "MER updated:"
        info $ "old: " <> show mer
        info $ "new: " <> show newMer
        info $ "minfo: " <> show mInfo
      let collectResults :: DL.DList (EReminderSupply, UTCTime) -> [(EReminderSupply, [UTCTime])]
          collectResults xs = convert <$> ys
            where
              ersToTyRep (ERS tp) = typeRep tp
              ys = groupBy ((==) `on` ersToTyRep . fst) $ DL.toList xs
              convert ts = (fst . head $ ts, snd <$> ts)
      -- scan through supplies and try restocking if an new event timestamp is found
      displayList <- collectResults <$>
        execWriterT (forM reminderSupplies (\e@(ERS tp) -> do
            let tyRep = typeRep tp
                -- always compute new supply (lazily)
                mNewSupply = renewSupply tp tzTokyo curTime
                cmp = comparing eventOccurTime
                rmOutdated xs =
                  case dropWhile isOutdatedEventReminder xs of
                    [] -> Nothing
                    ys -> Just ys
            -- cleanup step
            modify (first $ M.update rmOutdated tyRep)

            -- restock step
            mValue <- gets (M.lookup tyRep . fst)
            case mNewSupply of
              Nothing -> pure ()
              Just newSupply ->
                case mValue of
                  Nothing ->
                    modify (first $ M.insert tyRep [newSupply])
                  Just ers ->
                    -- skip restocking if timestamp mismatches
                    unless (hasBy cmp ers newSupply) $
                      modify (first $ M.adjust (insertSetBy cmp newSupply) tyRep)
            eventReminders <- fromMaybe [] <$> gets (M.lookup tyRep . fst)
            newEventReminders <-
              catMaybes <$> forM eventReminders (\er -> do
                -- if remindsDue contains anything, we should send current reminder
                let eot = eventOccurTime er
                    (remindsDue, mNewER) = getDuesByTime tThres er
                unless (null remindsDue) $ dTell (e, eot)
                pure mNewER
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

updateMER :: forall m. MonadWriter MessageRep m
          => UTCTime
          -> MaintenanceEventReminder
          -> MaintenanceInfo
          -> m MaintenanceEventReminder
updateMER curTime curERPair mInfo = do
    let (lCurER, rCurER) = curERPair
        (lInfo, rInfo) = mInfo
    (lNewER,rNewERPre) <-
      (,) <$> doUpdate lCurER lInfo
          <*> doUpdate rCurER rInfo
    let rNewER = case rNewERPre of
          Just (ez, xs)
            | er <- eventOccurTime ez
            , erds <- eventReminderDues ez
            , Just (ln,_) <- lNewER
            , erStart <- eventOccurTime ln
            {-
              here we want to do few things:
              - drop all due reminders that come before maintenance start
                it's just not interesting to know end time before it even gets started
              - make sure that we are able to announce end time the moment when
                the maintenance is started.
              also note that we don't check whether the list is empty for ER
              as: (1) the list is guaranteed to be non-empty
                  (2) end time must come after start time, so we won't end up
                      having an empty list
             -}
            , erds' <- (erStart `insertSet`)
                     . dropWhile (< erStart)
                     $ erds
            -> (,xs) <$> makeEventReminder er erds'
          _ -> rNewERPre
    (,) <$> stepMER curTime "Maintenance Start" lNewER
        <*> stepMER curTime "Maintenance End" rNewER
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
            Just (er, srcs) | eT <- eventOccurTime er ->
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
                -- we are safe as long as dueTimes is non-empty,
                -- which is true in this case.
                Just newER = makeEventReminder eventTime dueTimes
            -- TODO: use createEventReminderWithDueList
            pure (newER, srcs)
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
    Just (er, srcs)
      | eT <- eventOccurTime er
      -> do
      let (remindsDue, mNewER) = getDuesByTime tThres er
      unless (null remindsDue) $ dTell (desc, [(eT, srcs)])
      pure $ (,srcs) <$> mNewER
  where
    tThres = addUTCTime 20 curTime
