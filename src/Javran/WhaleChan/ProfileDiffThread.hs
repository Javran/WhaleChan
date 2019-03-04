{-# LANGUAGE DeriveGeneric #-}
module Javran.WhaleChan.ProfileDiffThread where

import Web.Twitter.Types
import Data.Time.Clock
import GHC.Generics
import Data.Aeson

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
  } deriving (Show, Generic)

instance FromJSON ProfileStat
instance ToJSON ProfileStat

data ProfileInfo
  = ProfileInfo
  { lastProfileImage :: Maybe URIString
  , lastStat :: Maybe (UTCTime, ProfileStat)
  } deriving (Show, Generic)

instance FromJSON ProfileInfo
instance ToJSON ProfileInfo
