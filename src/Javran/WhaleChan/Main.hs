{-# LANGUAGE LambdaCase #-}
module Javran.WhaleChan.Main
  ( main
  ) where

import System.Environment
import System.Exit
import Javran.WhaleChan.Base

main :: IO ()
main = getArgs >>= \case
  [cfg] -> do
    loadWEnv cfg >>= print
  _ -> putStrLn "WhaleChan <config.yaml>" >> exitFailure
