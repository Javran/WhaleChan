{-# LANGUAGE
    LambdaCase
  #-}
module Javran.WhaleChan.HealthThread
  ( healthThread
  , mkTcHealth
  , heartbeat
  , HealthRecord(..)
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.RWS
import Data.Time.Clock
import Text.Printf

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Javran.WhaleChan.Types
import Javran.WhaleChan.Util
import Javran.WhaleChan.Base
import qualified Javran.WhaleChan.Log as Log

{-

  This thread monitors other threads by
  consuming messages from other threads (aka heartbeat) on a regular basis,
  and print warning messages when threads haven't send heartbeat for a while.

  This thread will also try to restart other threads given that enough
  duration has passed without receiving a single heartbeat.
  The restarting works by sending threads in question an async exception.
  On the recipient thread it will be captured and hopefully restart itself.

 -}

mkTcHealth :: IO (MVar (Seq.Seq Heartbeat))
mkTcHealth = newMVar Seq.empty

heartbeat :: String -> Int -> WCM s ()
heartbeat who killTimeout  = do
    (_, TCommon{tcHealth=ch}) <- ask
    liftIO $ do
      t <- getCurrentTime
      tid <- myThreadId
      modifyMVar_ ch (pure . (Seq.|> Heartbeat who t tid killTimeout))

data HealthRecord
  = HealthRecord
  { hrThreadName :: String
  , hrKillTimeout :: Int -- in seconds, should be greater than longTimeThreshold
  , hrLastKnown :: UTCTime
  }

type HealthState =
  M.Map ThreadId HealthRecord

longTimeThreshold :: Num n => n
longTimeThreshold = 30 -- in second

healthThread :: WEnv -> IO ()
healthThread wenv@(_, TCommon{tcHealth=chan}) =
    evalStateT (forever loop) M.empty
  where
    loggerIO = wenvToLoggerIO wenv
    info = liftIO . Log.i' loggerIO "Health"
    warn = liftIO . Log.w' loggerIO "Health"

    loop :: StateT HealthState IO ()
    loop = do
        msgQueue <- liftIO $ swapMVar chan Seq.empty
        t <- liftIO getCurrentTime
        forM_ msgQueue $ \(Heartbeat who tMsg threadId kt) ->
          gets (M.lookup threadId) >>= \case
            Nothing -> do
              info $
                printf "%s is now known by name %s, with killTimeout=%d (t=%s)"
                   (show threadId)
                   who
                   kt
                   (show tMsg)
              modify (M.insert threadId (HealthRecord who kt tMsg))
            Just (HealthRecord _ ktPrev lkPrev) -> do
              -- note that thread name should never be changed.
              when (kt /= ktPrev) $
                info $
                  printf "Thread %s is changing killTimeout to %d"
                    who
                    kt
              when ((t `diffUTCTime` lkPrev) >= longTimeThreshold) $
                info $ who <> " is back online (t=" <> show tMsg <> ")"
              modify (M.insert threadId (HealthRecord who kt tMsg))
        records <- gets M.toAscList
        forM_ records $ \(threadId, HealthRecord who kt lk) -> do
          let timeDiff = t `diffUTCTime` lk
          when (timeDiff > longTimeThreshold) $
            if timeDiff < fromIntegral kt
              then
                warn $
                  "Haven't heard from " <> who <> " recently (last t="
                  <> show lk <> ")"
              else do
                {-
                  when a thread fails to send heartbeat for a while, try
                  killing that thread so it has a chance to come back online.
                 -}
                warn $
                  printf "Haven't heard from %s for too long, try killing the thread."
                    who
                modify (M.delete threadId)
                liftIO $ killThread threadId
        liftIO $ threadDelay $ 7 * oneSec
