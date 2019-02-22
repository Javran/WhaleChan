{-# LANGUAGE
    OverloadedStrings
  #-}
module Javran.WhaleChan.Log
  ( d, i, w, e
  ) where

{-
  Logging facility. This module is meant to be import qualified.
 -}

import Control.Monad.Logger

withLvl :: (MonadLogger m, ToLogStr a, ToLogStr b) => LogLevel -> a -> b -> m ()
withLvl lvl who msg = logWithoutLoc "" lvl (toLogStr who <> ": " <> toLogStr msg)

d,i,w,e :: MonadLogger m => String -> String -> m ()

d = withLvl LevelDebug
i = withLvl LevelInfo
w = withLvl LevelWarn
e = withLvl LevelError
