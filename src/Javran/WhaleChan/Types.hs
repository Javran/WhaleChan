{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , ExplicitForAll
  , ExistentialQuantification
  , KindSignatures
  , PolyKinds
  #-}
module Javran.WhaleChan.Types where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Web.Telegram.API.Bot as Tg
import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Series
import Data.Aeson
import Data.Typeable
import Data.Int (Int64)
import qualified Data.Text as T

data WEnv = WEnv
  { twConsumerKey :: BS.ByteString
  , twConsumerSecret :: BS.ByteString
  , twOAuthToken :: String
  , twOAuthSecret :: String
  , twWatchingUserId :: Int
  , twThreadStateFile :: FilePath
  , twTweetIdGreaterThan :: Integer
  , tgBotToken :: Tg.Token
  , tgChannelId :: Int64
  } deriving (Show)

instance ToJSON WEnv where
    toJSON WEnv {..}
      | Tg.Token tok <- tgBotToken
      = object [ "twitter-consumer-key" .= BSC.unpack twConsumerKey
               , "twitter-consumer-secret" .= BSC.unpack twConsumerSecret
               , "twitter-oauth-token" .= twOAuthToken
               , "twitter-oauth-secret" .= twOAuthSecret
               , "twitter-watching-user-id" .= twWatchingUserId
               , "twitter-thread-state-file" .= twThreadStateFile
               , "twitter-tweet-id-greater-than" .= twTweetIdGreaterThan
               , "telegram-bot-token" .= tok
               , "telegram-channel-id" .= tgChannelId
               ]

instance FromJSON WEnv where
    parseJSON = withObject "WEnv" $ \o ->
        WEnv
            <$> (BSC.pack <$> o .: "twitter-consumer-key")
            <*> (BSC.pack <$> o .: "twitter-consumer-secret")
            <*> o .: "twitter-oauth-token"
            <*> o .: "twitter-oauth-secret"
            <*> o .: "twitter-watching-user-id"
            <*> o .: "twitter-thread-state-file"
            <*> o .: "twitter-tweet-id-greater-than"
            <*> (Tg.Token <$> o .: "telegram-bot-token")
            <*> o .: "telegram-channel-id"

{-
  EventReminder contains info about the time the event will occur
  and a sorted list of times that a reminder is due.
 -}
data EventReminder = EventReminder
  { eventOccurTime :: UTCTime
  , eventReminderDues :: [UTCTime]
  } deriving (Show)

{-
  a ReminderSupply, when given current time,
  supplies a sorted list of times for the timer thread
 -}
class ReminderSupply (r :: k) where
    renewSupply :: forall p. p r -> TimeZoneSeries -> UTCTime -> EventReminder

data EReminderSupply =
  forall rs. (ReminderSupply rs, Typeable rs) => ERS (Proxy rs)

-- messages received by TelegramThread
data TgRxMsg
  = TgRMTimer T.Text -- sent from timer
  | TgRMTweetCreate T.Text -- sent from twitter
  | TgRMTweetDestroy Int -- sent from twitter
  deriving (Show)

newtype TwRxMsg
  = TwRMTgSent Int -- sent from telegram to notify that a message has been sent
