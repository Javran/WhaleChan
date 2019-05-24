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

import Control.Concurrent
import Control.Concurrent.Async
import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Olson
import Network.HTTP.Client (newManager, managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory
import System.Environment
import System.Exit

import Javran.WhaleChan.Base
import Javran.WhaleChan.TelegramThread (telegramThread)
import Javran.WhaleChan.ReminderThread (reminderThread)
import Javran.WhaleChan.TweetSyncThread (tweetSyncThread, createTwMVar)
import Javran.WhaleChan.ExtInfoThread (extInfoThread)
import Javran.WhaleChan.ProfileDiffThread (profileDiffThread)
import Javran.WhaleChan.ServerStatThread (serverStatThread)
import Javran.WhaleChan.HealthThread (healthThread, mkTcHealth)
import Javran.WhaleChan.Types
import qualified Javran.WhaleChan.Log as Log

startService :: WConf -> IO ()
startService wconf = do
  tcLogger <- newChan
  {-
    set default timeout to 23 seconds.
    we are using a prime number (at second precision)
    to reduce the amount of simultaneous computations.

    note that the timeout is not guaranteed (see ref below)
    so sometimes a thread won't be responding for a few seconds or even minutes,
    to deal with this situation, we need healthThread to keep an close eye on
    those thread (so far the problem only seems to raise in twitter-related
    network communications, so only TweetSyncThread and ProfileDiffThread is
    being monitored.

    ref regarding timeout not working:
    - https://github.com/snoyberg/http-client/issues/302
    - https://gitlab.haskell.org/ghc/ghc/issues/8684
   -}
  let mgrSettings =
        tlsManagerSettings
          { managerResponseTimeout = responseTimeoutMicro (23 * 1000 * 1000)
          }
  tcManager <- newManager mgrSettings
  tcTelegram <- newChan
  tcTwitter <- createTwMVar
  tcReminder <- newMVar (Nothing, Nothing)
  tcHealth <- mkTcHealth
  tcServerStat <- newMVar Nothing
  aLog <- async (startLogger tcLogger)
  -- ref: https://stackoverflow.com/q/43835656/315302
  tzTokyo <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/Asia/Tokyo"
  let wenv = (wconf,TCommon {..})
      workers =
          [ healthThread
          , reminderThread
          , telegramThread
          , tweetSyncThread
          , extInfoThread
          , profileDiffThread
          -- , serverStatThread
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
