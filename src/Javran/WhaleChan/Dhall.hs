{-# LANGUAGE
    DeriveGeneric
  #-}
module Javran.WhaleChan.Dhall where

import Dhall

-- This is the config loaded by Dhall,
-- which we will eventually turn into WConf.
data PreConf = PreConf
  { twConsumerKey :: Text
  , twConsumerSecret :: Text
  , twOAuthToken :: Text
  , twOAuthSecret :: Text
  , twWatchingUserId :: Natural
  , twIgnoreOlderThan :: Natural
  , tgBotToken :: Text
  , tgChannelId :: Integer
  } deriving (Show, Generic)

