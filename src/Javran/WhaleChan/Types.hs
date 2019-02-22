{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , ExplicitForAll
  , ExistentialQuantification
  , KindSignatures
  , PolyKinds
  , DeriveGeneric
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
import GHC.Generics
import Control.Monad.Logger

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as Tg
import qualified Data.Yaml as Yaml

data WConf = WConf
  { twConsumerKey :: BS.ByteString
  , twConsumerSecret :: BS.ByteString
  , twOAuthToken :: String
  , twOAuthSecret :: String
  , twWatchingUserId :: Int
  , twThreadStateFile :: FilePath
  , twIgnoreOlderThan :: Int
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
               , "twitter-ignore-older-than" .= twIgnoreOlderThan
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
            <*> o .: "twitter-ignore-older-than"
            <*> (Tg.Token <$> o .: "telegram-bot-token")
            <*> o .: "telegram-channel-id"

{-
  EventReminder contains info about the time the event will occur
  and a sorted list of times that a reminder is due.
 -}
data EventReminder = EventReminder
  { eventOccurTime :: UTCTime
  , eventReminderDues :: [UTCTime]
  } deriving (Show, Eq, Generic)

instance FromJSON EventReminder
instance ToJSON EventReminder

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
  = TgRMTimer T.Text (Maybe Tg.ParseMode) -- sent from timer
  | TgRMTweetCreate Integer T.Text -- sent from twitter
  | TgRMTweetDestroy Integer Int -- sent from twitter
  deriving (Show)

data TwRxMsg
  = TwRMTgSent Int Integer -- sent from telegram to notify that a message has been sent

-- note that for twitter thread we want a non-blocking
-- read operation, which is not available for Chan
-- so instead, we'll simulate a FIFO queue by holding Seq in MVar
type TwMVar = MVar (Seq.Seq TwRxMsg)

data WLog = WLog !UTCTime !LogLevel !LogStr

data TCommon
  = TCommon
  { tcTelegram :: Chan TgRxMsg -- channel used by telegram
  , tcTwitter :: TwMVar -- channel used by MVar
  , tcManager :: Manager -- share manager
  , tcLogger :: Chan WLog
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
    deriving (Show, Eq, Generic)

instance Yaml.ToJSON TgSyncState
instance Yaml.FromJSON TgSyncState

type TweetTracks = M.Map Integer (Status, TgSyncState)

type WCM s = LoggingT (RWST WEnv () s IO)

-- monad stack for twitter thread
type TweetSyncM = WCM TweetTracks

{-
  representation for a potentially partial range
  this type is for maintaining a consistent range for maintenance times,
  as parsers are only responsible for the syntactic wellformness
 -}
data PRange a
  = PL a -- a range that only has left side
  | PR a a -- a range that has both sides
    deriving (Show)

toPRange :: Ord a => Maybe a -> Maybe a -> Maybe (PRange a)
toPRange Nothing _ = Nothing
toPRange (Just x) Nothing = Just (PL x)
toPRange (Just x) (Just y)
  | x <= y = Just (PR x y)
  | otherwise = Just (PL x)

{-
  make type synonym of Dual of Endo
  this is to allow expressions that foldMap over a list
  to work from left to right instead of the traditional function composition
 -}
type DEndo a = Dual (Endo a)

appDEndo :: DEndo a -> (a -> a)
appDEndo = appEndo . getDual

mkDEndo :: (a -> a) -> DEndo a
mkDEndo = Dual . Endo
