{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , ExplicitForAll
  , ExistentialQuantification
  , KindSignatures
  , PolyKinds
  , DeriveGeneric
  , TypeFamilies
  , DefaultSignatures
  #-}
module Javran.WhaleChan.Types
  ( WConf(..)
  , TCommon(..)
  , WEnv
  , WCM
  , renewSupply
  , eventDescription
  , EReminderSupply(..)
  , EventReminder(..)
  , TgRxMsg(..)
  , TwRxMsg(..)
  , PRange
  , toPRange
  , DEndo
  , appDEndo
  , mkDEndo
  , Heartbeat(..)
  , WLog(..)
  , ReminderSupply
  , MaintenanceInfo
  , TweetTracks
  , TweetSyncM
  , TwMVar
  , TgSyncState(..)
  ) where

import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.RWS
import Data.Aeson
import Data.Int (Int64)
import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Series
import Data.Typeable
import GHC.Generics
import Web.Twitter.Conduit (Manager)
import Web.Twitter.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Web.Telegram.API.Bot as Tg

data WConf = WConf
  { twConsumerKey :: BS.ByteString
  , twConsumerSecret :: BS.ByteString
  , twOAuthToken :: String
  , twOAuthSecret :: String
  , twWatchingUserId :: Int
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
    eventDescription :: forall p. p r -> String

    default eventDescription :: (Typeable r) => p r -> String
    eventDescription p = show (typeRep p)

data EReminderSupply =
  forall rs. (ReminderSupply rs, Typeable rs) => ERS (Proxy rs)

-- messages received by TelegramThread
data TgRxMsg
  = TgRMTimer T.Text (Maybe Tg.ParseMode) -- sent from ReminderThread
  | TgRMTweetCreate Integer T.Text -- sent from TweetSyncThread
  | TgRMTweetDestroy Integer Int -- sent from TweetSyncThread
  | TgRMProfileImg T.Text -- sent from ProfileDiff
  | TgRMProfileStat T.Text -- sent from ProfileDiff
  deriving (Show)

data TwRxMsg
  = TwRMTgSent Int Integer -- sent from telegram to notify that a message has been sent

-- note that for twitter thread we want a non-blocking
-- read operation, which is not available for Chan
-- so instead, we'll simulate a FIFO queue by holding Seq in MVar
type TwMVar = MVar (Seq.Seq TwRxMsg)

data WLog = WLog !UTCTime !LogLevel !LogStr

{-
  maintenance produced by ExtInfoThread and consumed by ReminderThread
  the idea is to minimize the amount of work ReminderThread has to do
  (as it's time-sensitive), so we'll let ExtInfoThread summarize
  results grabbed from multiple sources and ReminderThread do the easy part of work
 -}
type MaintenanceInfo =
  ( Maybe (UTCTime, [String]) -- maintenance start time with confirming sources
  , Maybe (UTCTime, [String]) -- maintenance end time with confirming sources
  )

data Heartbeat = Heartbeat !String !UTCTime

data TCommon
  = TCommon
  { tcTelegram :: Chan TgRxMsg -- channel used by telegram
  , tcTwitter :: TwMVar -- channel used by MVar
  , tcManager :: Manager -- share manager
    -- INVARIANT: this should be full most of the time
    -- any readMVar should be followed by a putMVar to restore the full-ness
  , tcReminder :: MVar MaintenanceInfo -- mvar for reminder thread
  , tcLogger :: Chan WLog
  , tcHealth :: MVar (Seq.Seq Heartbeat)
  , tzTokyo :: TimeZoneSeries
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
type PRange a = Maybe (a,a)

toPRange :: Ord a => Maybe a -> Maybe a -> PRange a
toPRange mx my = do
    x <- mx
    y <- my
    guard (x <= y)
    pure (x,y)

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
