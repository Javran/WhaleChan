{-# LANGUAGE LambdaCase #-}
module Javran.WhaleChan.Main
  ( main
  ) where

import System.Environment
import System.Exit
import Javran.WhaleChan.Base

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

main :: IO ()
main = getArgs >>= \case
    [cfg] -> do
        loadWEnv cfg >>= print
    _ -> putStrLn "WhaleChan <config.yaml>" >> exitFailure
