module Javran.WhaleChan.Main
  ( main
  ) where

import Javran.WhaleChan.Env

main :: IO ()
main = print =<< getTWInfo
