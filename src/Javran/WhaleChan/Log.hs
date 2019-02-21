{-# LANGUAGE
    OverloadedStrings
  , LambdaCase
  #-}
module Javran.WhaleChan.Log
  (
  ) where

import System.IO
import Control.Monad.Logger
import System.Log.FastLogger
import Say
import Data.String

import Data.Time.Clock
import Data.Time.Format

logLevelToLogStr :: LogLevel -> LogStr
logLevelToLogStr = \case
    LevelDebug -> "D"
    LevelInfo -> "I"
    LevelWarn -> "W"
    LevelError -> "E"
    LevelOther {} -> "?"

utcTimeToLogStr :: UTCTime -> LogStr
utcTimeToLogStr = fromString . formatTime defaultTimeLocale fmtStr
  where
    fmtStr = iso8601DateFormat (Just "%T")

logCurrentMessage :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logCurrentMessage h _ _ lvl msg = do
    t <- utcTimeToLogStr <$> getCurrentTime
    let timedMsg = t <> "[" <> logLevelToLogStr lvl <> "] " <> msg
    pure () -- TODO
