{-# LANGUAGE
    LambdaCase
  , TypeApplications
  , NamedFieldPuns
  , RecordWildCards
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
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Control.Monad
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series

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

  - each reminder is implemented as a unique type implementing a common typeclass
  - reminder has an IO action that when executed with current time, computes
    a sorted list of pending reminders

    e.g. all info needed for:

    + "30 mins before [some event]"
    + "10 mins before [some event]"
    + "5 mins before [some event]"
    + "[some event] is happening"

  - we'll have a list of reminders implemented, visible to
    the reminder thread

  - reminder thread keeps track of a Map from TypeRep of reminders
    to a (potentially empty) list of reminds

  - the reminder thread is a loop that wakes up
    at (roughly) the beginning of every minute, then:

    + determine if it's time to remind something
    + send the post to telegram thread
    + after this is done, we'll have plenty of time dealing with the Map
    + discharge corresponding elements from the Map
    + reminder will have a channel for other threads to post computed new reminders to itself
    + for any Map value that is empty, we'll start a new thread with it's defined IO action to
      "restock" the list of reminders
    + by design we don't expect a reminder impl to return immediately (as it's IO)
      for example maintenance time would be co-referenced from multiple sources,
      which requires some amount of network traffic, which takes time.
    + perhaps maintenance time can be implemented specially, which ignores "restock" request
      and have it's own loop (say 20mins) and still post to reminder's channel
      as if a "restock" request is given.

  change of plan:

  - since most reminders are pure functions, using IO with dedicated threads, even lightwight once,
    is an overkill

  - instead, we'll have a thread to retrieve maintenance time and post to reminder thread
    when the result is available, this should be the only place that we'll interact with another thread

 -}

oneSec :: Int
oneSec = 1000000

oneMin :: Int
oneMin = oneSec * 60

-- infrastructure: support wake up at (roughly) begining of a miniute
timerThread :: IO ()
timerThread = forever $ do
    -- https://stackoverflow.com/a/8578237/315302
    t <- getCurrentTime
    -- compute millseconds since beginning of current minute
    let ms = round (fromIntegral oneSec * realToFrac @_ @Double (utctDayTime t)) `rem` oneMin
    -- wait to start of next minute
    threadDelay $ oneMin - ms
    t' <- getCurrentTime
    let timeRep = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%Q") t'
    putStrLn $ "Woke up at " ++ timeRep
-- TODO: use lens-datetime
localDayAdd :: Int -> LocalTime -> LocalTime
localDayAdd n lt@LocalTime{localDay = ld} = lt {localDay = addDays (fromIntegral n) ld}

nextPracticeReset :: LocalTime -> LocalTime
nextPracticeReset lt@LocalTime{localTimeOfDay = TimeOfDay {todHour}}
  | todHour < 3 = todayAt3
  | todHour < 15 = todayAt15
  | otherwise = localDayAdd 1 todayAt3
  where
    todayAt3 = lt {localTimeOfDay = TimeOfDay 3 0 0}
    todayAt15 = lt {localTimeOfDay = TimeOfDay 15 0 0}

nextDailyQuestReset :: LocalTime -> LocalTime
nextDailyQuestReset lt@LocalTime{localTimeOfDay = TimeOfDay {todHour}}
  | todHour < 5 = todayAt5
  | otherwise = localDayAdd 1 todayAt5
  where
    todayAt5 = lt {localTimeOfDay = TimeOfDay 5 0 0}

nextWeeklyQuestReset :: LocalTime -> LocalTime
nextWeeklyQuestReset lt@LocalTime{localDay, localTimeOfDay = TimeOfDay {todHour}}
  | dayOfWeek /= 1 = nextMondayAt5
  | todHour < 5 = todayAt5
  | otherwise = nextMondayAt5
  where
    todayAt5 = lt {localTimeOfDay = TimeOfDay 5 0 0}
    nextMondayAt5 = localDayAdd (8 - dayOfWeek) todayAt5
    (_, _, dayOfWeek) = toWeekDate localDay

startService :: WEnv -> IO ()
startService _ = do
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/Asia/Tokyo"
    tzPt <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/US/Pacific"
    t <- getCurrentTime
    let lTime = utcToLocalTime' tzs t
        pprLocalTime lt = do
          putStrLn $ "Japan:   " <> show (localDay lt) <> " " <> show (localTimeOfDay lt)
          let utcT = localTimeToUTC' tzs lt
              lt' = utcToLocalTime' tzPt utcT
          putStrLn $ "Pacific: " <> show (localDay lt') <> " " <> show (localTimeOfDay lt')
    putStrLn "# Current time"
    pprLocalTime lTime
    putStrLn "# Next PvP Reset"
    pprLocalTime (nextPracticeReset lTime)
    putStrLn "# Next Daily Quest Reset"
    pprLocalTime (nextDailyQuestReset lTime)
    putStrLn "# Next Weekly Quest Reset"
    pprLocalTime (nextWeeklyQuestReset lTime)


{-
  events to be implemented:

  - daily quest reset
  - practice reset
  - senka accounting (3 times at the end of each month)
  - EO reset
  - quest senka freeze
  - secretary & comment accounting
  - weekly quest reset
  - monthly quest reset
  - quaterly quest reset: 3 6 9 12

 -}

-- ref: https://stackoverflow.com/q/43835656/315302

main :: IO ()
main = getArgs >>= \case
    [cfg] -> do
        loadWEnv cfg >>= startService
    _ -> putStrLn "WhaleChan <config.yaml>" >> exitFailure
