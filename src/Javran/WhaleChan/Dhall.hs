{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , NamedFieldPuns
  , DeriveGeneric
  #-}
module Javran.WhaleChan.Dhall where

import Dhall
import Web.Twitter.Conduit
import Data.Text.Encoding (encodeUtf8)

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

getTwInfo :: PreConf -> TWInfo
getTwInfo PreConf{..} = def { twToken, twProxy }
  where
    twOAuth =
        twitterOAuth
        { oauthConsumerKey = encodeUtf8 twConsumerKey
        , oauthConsumerSecret = encodeUtf8 twConsumerSecret
        }
    twCredential =
        Credential
        [ ("oauth_token", encodeUtf8 twOAuthToken)
        , ("oauth_token_secret", encodeUtf8 twOAuthSecret)
        ]
    twToken = def { twOAuth, twCredential }
    twProxy = Nothing
