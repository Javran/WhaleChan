module Javran.WhaleChan.ExtInfoThread where

import Data.Time.Clock

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import Network.HTTP.Client

import Javran.WhaleChan.Types

import qualified Javran.WhaleChan.FromSource.KcsConst as KcsConst
import qualified Javran.WhaleChan.FromSource.Kc3Kai as Kc3Kai
import qualified Javran.WhaleChan.FromSource.Wikia as Wikia
import qualified Javran.WhaleChan.FromSource.Kcwiki as Kcwiki

{-
  thread for getting info from external sources
 -}

data ExtInfo
  = ExtInfo
  { maintenanceTimes :: M.Map String (PRange UTCTime)
  , serverInfo :: IM.IntMap String
  }

oneSec :: Int
oneSec = 1000000

{-
  external sources except KcsConst.

  - as KcsConst also contains server info,
    we want to give it a special treatment
 -}
sources :: [(String, Manager -> IO (Maybe (PRange UTCTime)))]
sources =
    [ ("Kc3Kai", Kc3Kai.getInfo)
    , ("Wikia", Wikia.getInfo)
    , ("Kcwiki", Kcwiki.getInfo)
    ]

extInfoThread :: WEnv -> IO ()
extInfoThread wenv = pure ()
