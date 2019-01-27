{-# LANGUAGE
    LambdaCase
  , TypeApplications
  , NamedFieldPuns
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
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import Say

import Javran.WhaleChan.ReoccuringEvents

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

timerThread :: IO ()
timerThread = forever $ do
    waitUntilStartOfNextMinute
    t' <- getCurrentTime
    let timeRep = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%Q") t'
    sayString $ "Woke up at " ++ timeRep
-- TODO: use lens-datetime

testService :: WEnv -> IO ()
testService _ = do
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
    putStrLn "# Next Monthly Quest Reset"
    pprLocalTime (nextMonthlyQuestReset lTime)
    putStrLn "# Next Quarterly Quest Reset"
    pprLocalTime (nextQuarterlyQuestReset lTime)
    putStrLn "# Next EO Reset"
    pprLocalTime (nextExtraOperationReset lTime)
    putStrLn "# Next Senka Accounting"
    pprLocalTime (nextSenkaAccounting lTime)
    putStrLn "# Next Quest Point Deadline"
    pprLocalTime (nextQuestPointDeadline lTime)

startService :: WEnv -> IO ()
startService _ = do
  aTimer <- async timerThread
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
