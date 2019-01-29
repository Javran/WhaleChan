{-# LANGUAGE
    LambdaCase
  , TypeApplications
  , NamedFieldPuns
  , LambdaCase
  , ScopedTypeVariables
  , DataKinds
  #-}
module Javran.WhaleChan.Main
  ( timerThread
  , main
  ) where

import System.Environment
import System.Exit
import Javran.WhaleChan.Types
import Javran.WhaleChan.Base

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Data.Time.Clock
import Data.Time.Format
import Control.Monad
import Control.Monad.Writer
import Data.Monoid
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import Say
import Data.Proxy
import Data.Typeable
import Data.Maybe
import Control.Monad.State

import Javran.WhaleChan.ReminderSupply
import qualified Data.Map.Strict as M

{-
  architecture draft:

  twitter api thread

  - for each 10 seconds, retrieve without since_id and count=200

    + the rate limit is 900 for a 15min window, we'll be using 6 per min * 15 = 90 calls,
      way below the limit for a decent response time.
    + the intention of not using since_id is to retrieve tweet ids
      of the past so that deletion can be detected
    + talk to telegram api thread when there's a need of sending messages

  telegram api thread

  - assume no user interaction, the mere task is to post to the channel
  - wait for messages to arrive and then send

  scheduler thread

  - for reminder of reoccurring events
  - 30 mins before
  - 5 mins before
  - right now

  main thread into a sleep loop, check thread health occasionally

 -}

{-
  reminder impl draft:

  - concepts:

    + a EventReminder contains a sorted list of pending reminders
      around a particular event.
    + several type implements ReminderSupply, which, when called with
      current time generates a corresponding EventReminder.
      (NOTE: this is a pure function, we'll worry about parsing maintenance times later)
    + these types are promoted data types, unified by a common kind: ReminderSupplier

- we'll have a list of reminders implemented, visible to
  the reminder thread

- the timer thread keeps track of a Map from TypeRep of ReminderSupplier
  to EventReminder

- when EventReminder cannot be found or contains an empty list of eventReminderDues,
  timer thread will attempt to "resupply" by calling corresponding reminder.

- we'll basically reminder the following events:

    + "30 mins before [some event]"
    + "10 mins before [some event]"
    + "5 mins before [some event]"
    + "[some event] is happening"

  but this is by no meaning a limit, the definition of ReminderSupply
  is flexible to allow time much more longer - for example

- the reminder thread is a loop that:

  + wakes up at (roughly) the beginning of every minute
  + then determine if it's time to remind something
  + send the post to telegram thread
  + after this is done, we'll try to "resupply" EventReminder that are empty
  + for dealing with maintenance times, we'll have a dummy ReminderSupplier
    that does nothing upon resupply - a dedicated thread will check and parse
    maintenance time on a regular basis and supply timer thread with result,
    instead of doing passive "resupply"

 -}

oneSec :: Int
oneSec = 1000000

oneMin :: Int
oneMin = oneSec * 60

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
    -- threadDelay $ oneMin - ms
    -- TODO: we only wait for 5s now for it's easier to debug
    threadDelay (oneSec * 5)

reminderSupplies :: [EReminderSupply]
reminderSupplies =
    [ ERS (Proxy :: Proxy 'DailyQuestReset)
    , ERS (Proxy :: Proxy 'WeeklyQuestReset)
    , ERS (Proxy :: Proxy 'MonthlyQuestReset)
    , ERS (Proxy :: Proxy 'QuarterlyQuestReset)
    , ERS (Proxy :: Proxy 'ExtraOperationReset)
    , ERS (Proxy :: Proxy 'SenkaAccounting)
    , ERS (Proxy :: Proxy 'QuestPointDeadline)
    ]

type TimerM a = StateT (M.Map TypeRep EventReminder) IO a

timerThread :: TimerM ()
timerThread = do
    tzPt <- liftIO $ getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/US/Pacific"
    tzs <- liftIO $ getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/Asia/Tokyo"
    -- TODO: in future we'll need to persistent the thread state
    -- INVARIANT: all values of the State Map shouldn't
    -- contain EventReminder whose eventReminderDues is empty

    -- note: cleaning-up here might not be a good idea as
    -- it might lead to some reminder times being discharged without being
    -- considered

    -- into infinite loop
    forever $ do
      -- wait until the start of next minute
      t' <- liftIO (waitUntilStartOfNextMinute >> getCurrentTime)
      let timeRep = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%Q") t'
          -- any event in future within 20 seconds is also included
          -- this assume that we can always process all reminders within 20 seconds
          -- which should be way more than enough.
          tThres = addUTCTime 20 t'
      liftIO $ sayString $ "Woke up at " ++ timeRep
      -- scan & update reminders, and collect things needed to be displayed
      displayList <- (catMaybes <$>) . forM reminderSupplies $ \e@(ERS tp) ->
        let tyRep = typeRep tp
        in gets (M.lookup tyRep) >>= \case
            Nothing -> pure Nothing
            Just (EventReminder eot erds) -> do
              -- if remindsDue contains anything, we should send current reminder
              let (remindsDue, erds') = span (< tThres) erds
                  newVal = if null erds'
                    then Nothing
                    else Just (EventReminder eot erds')
              modify (M.update (const newVal) tyRep)
              pure $ if null remindsDue
                then Nothing
                else Just (e, eot)
      -- process display, IO is sufficient
      liftIO $ forM_ displayList $ \(ERS tp, eTime) -> do
        let tyRep = typeRep tp
        sayString $ "Reminder: " <> show tyRep
        let lt = utcToLocalTime' tzs eTime
            lt' = utcToLocalTime' tzPt eTime
            pprTime seconds
              | seconds > 3600 =
                  let (hh,ss') = seconds `divMod` 3600
                      (mm,ss) = ss' `divMod` 60
                  in show hh <> " hours " <> show mm <> " minutes " <> show ss <> " seconds"
              | seconds > 60 =
                  let (mm,ss) = seconds `divMod` 60
                  in show mm <> " minutes " <> show ss <> " seconds"
              | otherwise = show seconds <> " seconds"
        sayString $ "  Remaining time: " <> pprTime (floor (eTime `diffUTCTime` t') :: Int)
        sayString $ "  Japan:   " <> show (localDay lt) <> " " <> show (localTimeOfDay lt)
        sayString $ "  Pacific: " <> show (localDay lt') <> " " <> show (localTimeOfDay lt')
      -- re-stock EventReminder here
      -- we've been careful to make sure EventReminder whose erds is empty
      -- is removed instead of being kept. which means we just need to restock those
      -- that returns Nothing by lookup
      tDone <- liftIO getCurrentTime
      let restock (ERS tp) = Endo (M.alter altVal tyRep)
            where
              altVal x = case x of
                Nothing -> Just (renewSupply tp tzs tDone)
                Just _ -> x
              tyRep = typeRep tp
      modify (appEndo (foldMap restock reminderSupplies))

-- TODO: use lens-datetime

startService :: WEnv -> IO ()
startService _ = do
  aTimer <- async (evalStateT timerThread M.empty)
  wait aTimer

{-
  events to be implemented:

  - [x] daily quest reset
  - [x] practice reset
  - [x] senka accounting (3 times at the end of each month)
  - [x] EO reset
  - [x] quest senka freeze
  - [ ] secretary & comment accounting
  - [x] weekly quest reset
  - [x] monthly quest reset
  - [x] quaterly quest reset: 3 6 9 12

 -}

-- ref: https://stackoverflow.com/q/43835656/315302

main :: IO ()
main = getArgs >>= \case
    [cfg] -> loadWEnv cfg >>= startService
    _ -> putStrLn "WhaleChan <config.yaml>" >> exitFailure
