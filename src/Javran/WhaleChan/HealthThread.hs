{-# LANGUAGE
    LambdaCase
  #-}
module Javran.WhaleChan.HealthThread
  ( healthThread
  , mkTcHealth
  , heartbeat
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.RWS
import Data.Time.Clock

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Javran.WhaleChan.Types
import Javran.WhaleChan.Util
import Javran.WhaleChan.Base
import qualified Javran.WhaleChan.Log as Log

mkTcHealth :: IO (MVar (Seq.Seq Heartbeat))
mkTcHealth = newMVar Seq.empty

heartbeat :: String -> WCM s ()
heartbeat who = do
    (_, TCommon{tcHealth=ch}) <- ask
    liftIO $ do
      t <- getCurrentTime
      tid <- myThreadId
      modifyMVar_ ch (pure . (Seq.|> Heartbeat who t tid))

{-
  TODO:

  - when a thread fails to send heartbeat for a while, try
    killing that thread so it has a chance to come back online.
 -}

healthThread :: WEnv -> IO ()
healthThread wenv@(_, TCommon{tcHealth=chan}) =
    evalStateT (forever loop) M.empty
  where
    loggerIO = wenvToLoggerIO wenv
    info = liftIO . Log.i' loggerIO "Health"
    warn = liftIO . Log.w' loggerIO "Health"

    loop :: StateT (M.Map String UTCTime) IO ()
    loop = do
        msgQueue <- liftIO $ swapMVar chan Seq.empty
        t <- liftIO getCurrentTime
        forM_ msgQueue $ \(Heartbeat who tMsg _tid) ->
          gets (M.lookup who) >>= \case
            Nothing -> do
              info $
                "Heard from " <> who <> " for the first time (t="
                <> show tMsg <> ")"
              modify (M.insert who tMsg)
            Just _ -> do
              when ((t `diffUTCTime` tMsg) >= 30) $
                info $ who <> " is back online (t=" <> show tMsg <> ")"
              modify (M.insert who tMsg)
        records <- gets M.toAscList
        forM_ records $ \(who, lastTime) ->
          when (t `diffUTCTime` lastTime > 30) $
            warn $
              "Haven't heard from " <> who <> " recently (last t="
              <> show lastTime <> ")"
        liftIO $ threadDelay $ 7 * oneSec
