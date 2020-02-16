{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , NamedFieldPuns
  , DeriveGeneric
  #-}
module Javran.WhaleChan.Dhall (loadWConf) where

import Dhall
import Data.Text.Encoding (encodeUtf8)
import Web.Twitter.Conduit

import Javran.WhaleChan.Types

import qualified Web.Telegram.API.Bot as Tg

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

instance FromDhall PreConf

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

toWConf :: PreConf -> WConf
toWConf pc@PreConf {..} =
    WConf
    { twInfo = getTwInfo pc
    , twWatchingUserId = fromIntegral twWatchingUserId
    , twIgnoreOlderThan = fromIntegral twIgnoreOlderThan
    , tgBotToken = Tg.Token tgBotToken
    , tgChannelId = fromIntegral tgChannelId
    }

loadWConf :: FilePath -> IO WConf
loadWConf fp = toWConf <$> inputFile auto fp
