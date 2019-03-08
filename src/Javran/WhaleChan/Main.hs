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
import Control.Concurrent
import Data.Time.Clock

import Javran.WhaleChan.Base
import Javran.WhaleChan.TelegramThread (telegramThread)
import Javran.WhaleChan.ReminderThread (reminderThread)
import Javran.WhaleChan.TweetSyncThread (tweetSyncThread, createTwMVar)
import Javran.WhaleChan.ExtInfoThread (extInfoThread)
import Javran.WhaleChan.ProfileDiffThread (profileDiffThread)
import Javran.WhaleChan.HealthThread
import Javran.WhaleChan.Types
import qualified Javran.WhaleChan.Log as Log

{-
  TODO

  [ ] dev twitter icon change detection
  [ ] maintenance time reminder
  [ ] kcs2/version.json could be used to detect server availability?
  [ ] track daily twitter follower change

 -}
startService :: WConf -> IO ()
startService wconf = do
  tcLogger <- newChan
  tcManager <- newManager tlsManagerSettings
  tcTelegram <- newChan
  tcTwitter <- createTwMVar
  tcReminder <- newMVar (Nothing, Nothing)
  tcHealth <- mkTcHealth
  aLog <- async (startLogger tcLogger)
  let wenv = (wconf,TCommon {..})
      workers =
          [ healthThread
          , reminderThread
          , telegramThread
          , tweetSyncThread
          , extInfoThread
          , profileDiffThread
          ]
  let info = Log.i' (wenvToLoggerIO wenv) "Main"
  t <- getCurrentTime
  cNum <- getNumCapabilities
  info $ "WhaleChan started at " <> show t
  info $ "capability=" <> show cNum
  {-
    note that all threads are supposed to run forever
    unless too many error happens, so it actually doesn't matter
    which one we are waiting on.
   -}
  mapM (async . ($ wenv)) workers >>= mapM_ wait
  wait aLog

-- config file name is implicit, see Javran.WhaleChan.Base
main :: IO ()
main = getArgs >>= \case
    [workingDir] ->
      setCurrentDirectory workingDir >>
      loadWEnv >>= startService
    _ -> putStrLn "WhaleChan <working_dir>" >> exitFailure
