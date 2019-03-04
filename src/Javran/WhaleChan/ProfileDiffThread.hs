{-# LANGUAGE
    DeriveGeneric
  , TypeApplications
  #-}
module Javran.WhaleChan.ProfileDiffThread where

import Web.Twitter.Types
import Data.Time.Clock
import GHC.Generics
import Data.Aeson
import Data.Default
import Control.Concurrent
import Control.Monad.RWS

import Javran.WhaleChan.Types
import Javran.WhaleChan.Base
import Javran.WhaleChan.Util
import qualified Javran.WhaleChan.Log as Log

{-
  (draft)
  This thread detects profile image change
  and also sends a daily message about changes of:

  - status count
  - following count
  - follower count

  also let's say the daily message is sent around 00:00 JST.
 -}

data ProfileStat
  = ProfileStat
  { statusCount :: Int
  , followingCount :: Int
  , followerCount :: Int
  } deriving (Show, Generic, Eq)

instance FromJSON ProfileStat
instance ToJSON ProfileStat

data ProfileInfo
  = ProfileInfo
  { lastProfileImage :: Maybe URIString
  , lastStat :: Maybe (UTCTime, ProfileStat)
  } deriving (Show, Generic, Eq)

instance Default ProfileInfo
instance FromJSON ProfileInfo
instance ToJSON ProfileInfo

type M = WCM ProfileInfo

profileDiffThread :: WEnv -> IO ()
profileDiffThread wenv =
    autoWCM @ProfileInfo "ProfileDiff" "profile-diff.yaml" wenv $ \markStart -> do
        markEnd <- markStart
        Log.i "ProfileDiff" "tick"
        markEnd
        liftIO $ threadDelay $ oneSec * 5
