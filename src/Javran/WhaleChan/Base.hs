module Javran.WhaleChan.Base
  ( loadWEnv
  ) where

import Javran.WhaleChan.Types
import qualified Data.Yaml as Yaml

loadWEnv :: FilePath -> IO WEnv
loadWEnv = Yaml.decodeFileThrow
