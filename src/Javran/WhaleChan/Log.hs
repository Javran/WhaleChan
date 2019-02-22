{-# LANGUAGE
    OverloadedStrings
  #-}
module Javran.WhaleChan.Log
  ( d, i, w, e
  , d', i', w', e'
  , LoggerIO
  ) where

{-
  Logging facility. This module is meant to be import qualified.
 -}

import Control.Monad.Logger

type LoggerIO = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

withLvl :: (MonadLogger m, ToLogStr a, ToLogStr b) => LogLevel -> a -> b -> m ()
withLvl lvl who msg = logWithoutLoc "" lvl (toLogStr who <> ": " <> toLogStr msg)

withLvlIO :: (ToLogStr a, ToLogStr b)
          => LogLevel
          -> LoggerIO -> a -> b -> IO ()
withLvlIO lvl loggerIO who msg =
    runLoggingT (withLvl lvl who msg) loggerIO

d,i,w,e :: MonadLogger m => String -> String -> m ()

d = withLvl LevelDebug
i = withLvl LevelInfo
w = withLvl LevelWarn
e = withLvl LevelError

d',i',w',e' :: LoggerIO -> String -> String -> IO ()

d' = withLvlIO LevelDebug
i' = withLvlIO LevelInfo
w' = withLvlIO LevelWarn
e' = withLvlIO LevelError
