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
import Javran.WhaleChan.TwitterThread (twitterThread)
import Javran.WhaleChan.Types

{-
  architecture:

  - TelegramThread listens to a multithread channel
    and send messages accordingly
  - TimerThread for signaling time-related events
  - TwitterThread (TODO) for monitoring @kancolle_staff for changes

 -}

startService :: WEnv -> IO ()
startService wenv = do
  mgr <- newManager tlsManagerSettings
  ch <- newChan
  let WEnv {tgBotToken=botToken, tgChannelId=tgChannelId} = wenv
  aTimer <- async (evalStateT (timerThread ch) M.empty)
  aTg <- async (telegramThread mgr ch botToken tgChannelId)
  aTw <- async (twitterThread mgr wenv)
  wait aTimer
  wait aTg
  wait aTw

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
