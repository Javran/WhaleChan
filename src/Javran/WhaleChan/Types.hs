{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Javran.WhaleChan.Types where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Web.Telegram.API.Bot as Tg
import Data.Aeson

data WEnv = WEnv
  { twConsumerKey :: BS.ByteString
  , twConsumerSecret :: BS.ByteString
  , twOAuthToken :: String
  , twOAuthSecret :: String
  , tgBotToken :: Tg.Token
  } deriving (Show)

instance ToJSON WEnv where
  toJSON WEnv {..}
    | Tg.Token tok <- tgBotToken
    = object [ "twitter-consumer-key" .= BSC.unpack twConsumerKey
             , "twitter-consumer-secret" .= BSC.unpack twConsumerSecret
             , "twitter-oauth-token" .= twOAuthToken
             , "twitter-oauth-secret" .= twOAuthSecret
             , "telegram-bot-token" .= tok
             ]

instance FromJSON WEnv where
  parseJSON = withObject "WEnv" $ \o ->
      WEnv
        <$> (BSC.pack <$> o .: "twitter-consumer-key")
        <*> (BSC.pack <$> o .: "twitter-consumer-secret")
        <*> o .: "twitter-oauth-token"
        <*> o .: "twitter-oauth-secret"
        <*> (Tg.Token <$> o .: "telegram-bot-token")