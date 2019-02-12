module Javran.WhaleChan.Base
  ( loadWEnv
  ) where

import Javran.WhaleChan.Types
import qualified Data.Yaml as Yaml

{-
  it is assumed that all files related to the current
  running instance is located in current directory.
  the main program will accept a path to that directory
  and switch immediately to it at startup.
  all proceeding operations on files should just use a predefined name
  without using subdirectories.

  - config.yaml: config file

 -}

loadWEnv :: IO WEnv
loadWEnv = Yaml.decodeFileThrow "config.yaml"
