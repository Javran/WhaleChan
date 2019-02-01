{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , OverloadedStrings
  #-}
module Javran.WhaleChan.Main
  ( timerThread
  , main
  ) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad.State
import qualified Data.Map.Strict as M
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment
import System.Exit

import Javran.WhaleChan.Base
import Javran.WhaleChan.TimerThread (timerThread)
import Javran.WhaleChan.TelegramThread (telegramThread)
import Javran.WhaleChan.Types

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

startService :: WEnv -> IO ()
startService wenv = do
  mgr <- newManager tlsManagerSettings
  ch <- newChan
  let WEnv {tgBotToken=botToken, tgChannelId=tgChannelId} = wenv
  aTimer <- async (evalStateT (timerThread ch) M.empty)
  aTg <- async (telegramThread mgr ch botToken tgChannelId)
  wait aTimer
  wait aTg

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
