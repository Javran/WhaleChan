{-# LANGUAGE
    LambdaCase
  , TypeApplications
  #-}
module Javran.WhaleChan.Main
  ( main
  ) where

import System.Environment
import System.Exit
import Javran.WhaleChan.Types
import Javran.WhaleChan.Base

import Control.Concurrent (threadDelay)
import Data.Time.Clock
import Data.Time.Format
import Control.Monad

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


oneSec :: Int
oneSec = 1000000

oneMin :: Int
oneMin = oneSec * 60

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

startService :: WEnv -> IO ()
startService _ = timerThread
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


main :: IO ()
main = getArgs >>= \case
    [cfg] -> do
        loadWEnv cfg >>= startService
    _ -> putStrLn "WhaleChan <config.yaml>" >> exitFailure
