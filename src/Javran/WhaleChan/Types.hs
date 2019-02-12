{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , ExplicitForAll
  , ExistentialQuantification
  , KindSignatures
  , PolyKinds
  #-}
module Javran.WhaleChan.Types where

import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Series
import Data.Aeson
import Data.Typeable
import Data.Int (Int64)
import Control.Concurrent
import Control.Monad.RWS
import Web.Twitter.Types
import Web.Twitter.Conduit (Manager)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as Tg

data WConf = WConf
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

instance ToJSON WConf where
    toJSON WConf {..}
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

instance FromJSON WConf where
    parseJSON = withObject "WEnv" $ \o ->
        WConf
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
  | TgRMTweetCreate Integer T.Text -- sent from twitter
  | TgRMTweetDestroy Integer Int -- sent from twitter
  deriving (Show)

data TwRxMsg
  = TwRMTgSent Int Integer -- sent from telegram to notify that a message has been sent

-- note that for twitter thread we want a non-blocking
-- read operation, which is not available for Chan
-- so instead, we'll simulate a FIFO queue by holding Seq in MVar
type TwMVar = MVar (Seq.Seq TwRxMsg)

data TCommon
  = TCommon
  { tcTelegram :: Chan TgRxMsg -- channel used by telegram
  , tcTwitter :: TwMVar -- channel used by MVar
  , tcManager :: Manager -- share manager
  }

-- Runtime enviroment share among threads
type WEnv = (WConf, TCommon)

-- for keeping track of sync-state between twitter thread and telegram thread
data TgSyncState
  = TSPending -- indicate that a tweet is detected but not yet sent to the channel
  | TSSynced Int -- indicate that a tweet is already sent as a telegram message
  | TSTimedOut -- tweets acknowledged without syncing to tg (<= tweet-id-greater-than)
  | TSRemoving Int -- indicate that a tweet is removed but channel is not yet notified
  | TSRemoved -- indicate that a tweet is removed and ack-ed with another tg message.
      Int {- first one is for the existing tg msg id -}
      Int {- tg msg id that informs about deletion-}
  | TSDrop -- a TSPending message is not yet synced, so its deleted form has to be dropped
    deriving (Show)

type TweetTracks = M.Map Integer (Status, TgSyncState)

-- TODO: one monad to rule them all? (shared env with independent state for each thread)
type WCM s = RWST WEnv () s IO

-- monad stack for twitter thread
type TwM = WCM TweetTracks
