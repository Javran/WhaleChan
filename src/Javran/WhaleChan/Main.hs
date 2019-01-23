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

localDayAdd :: Integer -> LocalTime -> LocalTime
localDayAdd n lt@LocalTime{localDay = ld} = lt {localDay = addDays n ld}

nextDailyQuestReset :: LocalTime -> LocalTime
nextDailyQuestReset lt@LocalTime{localTimeOfDay = TimeOfDay {todHour}}
  | todHour < 5 = today5pm
  | otherwise = localDayAdd 1 today5pm
  where
    today5pm = lt {localTimeOfDay = TimeOfDay 5 0 0}

startService :: WEnv -> IO ()
startService _ = do
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/Asia/Tokyo"
    t <- getCurrentTime
    let lTime = utcToLocalTime' tzs t
        pprLocalTime LocalTime{..} = do
          putStrLn $ "Day: " <> show localDay
          putStrLn $ "Time: " <> show localTimeOfDay
    putStrLn "# Current time"
    pprLocalTime lTime
    putStrLn "# Next Daily Quest Reset"
    pprLocalTime (nextDailyQuestReset lTime)

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