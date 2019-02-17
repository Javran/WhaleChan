{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , OverloadedStrings
  , RecordWildCards
  #-}
module Javran.WhaleChan.Main
  ( main
  ) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory
import System.Environment
import System.Exit

import Javran.WhaleChan.Base
import Javran.WhaleChan.TelegramThread (telegramThread)
import Javran.WhaleChan.TimerThread (reminderThread)
import Javran.WhaleChan.TwitterThread (tweetSyncThread, createTwMVar)
import Javran.WhaleChan.Types

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

-- config file name is implicit, see Javran.WhaleChan.Base
main :: IO ()
main = getArgs >>= \case
    [workingDir] ->
      setCurrentDirectory workingDir >>
      loadWEnv >>= startService
    _ -> putStrLn "WhaleChan <working_dir>" >> exitFailure
