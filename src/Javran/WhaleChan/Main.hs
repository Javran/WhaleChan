{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , OverloadedStrings
  , RecordWildCards
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
import System.Directory

import Javran.WhaleChan.Base
import Javran.WhaleChan.TimerThread (timerThread, reminderThread)
import Javran.WhaleChan.TelegramThread (telegramThread)
import Javran.WhaleChan.TwitterThread (tweetSyncThread, createTwMVar)
import Javran.WhaleChan.Types

{-
  architecture:

  - TelegramThread listens to a multithread channel
    and send messages accordingly
  - TimerThread for signaling time-related events
  - TwitterThread (TODO) for monitoring @kancolle_staff for changes

 -}

startService :: WConf -> IO ()
startService wconf = do
  tcManager <- newManager tlsManagerSettings
  tcTelegram <- newChan
  tcTwitter <- createTwMVar
  let tcomm = TCommon {..}
      WConf {tgBotToken=botToken, tgChannelId=tgChannelId} = wconf
      wenv = (wconf, tcomm)
  aTimer <- async (reminderThread wenv)
  aTg <- async (telegramThread tcManager tcTelegram tcTwitter botToken tgChannelId)
  aTw <- async (tweetSyncThread wenv)
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

-- config file name is implicit, see Javran.WhaleChan.Base
main :: IO ()
main = getArgs >>= \case
    [workingDir] ->
      setCurrentDirectory workingDir >>
      loadWEnv >>= startService
    _ -> putStrLn "WhaleChan <working_dir>" >> exitFailure
