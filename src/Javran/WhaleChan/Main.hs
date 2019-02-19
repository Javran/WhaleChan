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
import Javran.WhaleChan.TweetSyncThread (tweetSyncThread, createTwMVar)
import Javran.WhaleChan.NextMaintenance
import Javran.WhaleChan.Types

{-
  TODO

  [ ] dev twitter icon change detection
  [ ] maintenance time reminder
  [ ] kcs2/version.json could be used to detect server availability?
  [ ] track daily twitter follower change

 -}
startService :: WConf -> IO ()
startService wconf = do
  tcManager <- newManager tlsManagerSettings
  tcTelegram <- newChan
  tcTwitter <- createTwMVar
  let wenv = (wconf,TCommon {..})
  aTimer <- async (reminderThread wenv)
  aTg <- async (telegramThread wenv)
  aTw <- async (tweetSyncThread wenv)
  getInfoFromGameSource wenv
  getInfoFromKC3Kai wenv
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
