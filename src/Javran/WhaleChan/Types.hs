{-# LANGUAGE
    DeriveGeneric
  #-}
module Javran.WhaleChan.Types
  ( WConf(..)
  , TCommon(..)
  , WEnv
  , WCM
  , TgRxMsg(..)
  , TwRxMsg(..)
  , PRange
  , toPRange
  , DEndo
  , appDEndo
  , mkDEndo
  , Heartbeat(..)
  , WLog(..)
  , MaintenanceInfo
  , TweetTracks
  , TweetSyncM
  , TwMVar
  , TgSyncState(..)
  ) where

import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.RWS
import Data.Int (Int64)
import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Series
import GHC.Generics
import Web.Twitter.Conduit
import Web.Twitter.Types

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Web.Telegram.API.Bot as Tg

data WConf = WConf
  { twInfo :: TWInfo
  , twWatchingUserId :: Int
  , twIgnoreOlderThan :: Int
  , tgBotToken :: Tg.Token
  , tgChannelId :: Int64
    {-
      This option acts as a "feature switch",
      thread workers that are still in development can choose to
      do thing differently depending on this flag.
     -}
  , isDevMode :: Bool
  }

-- messages received by TelegramThread
data TgRxMsg
  = TgRMTimer T.Text (Maybe Tg.ParseMode) -- sent from ReminderThread
    {-
       sent from TweetSyncThread:

       TgRMTweetCreate <tweet id> <content> <should preview?>

     -}
  | TgRMTweetCreate Integer T.Text Bool --
  | TgRMTweetDestroy Integer Int -- sent from TweetSyncThread
  | TgRMProfileImg T.Text -- sent from ProfileDiff
  | TgRMProfileStat T.Text -- sent from ProfileDiff
  | TgRMServerStat T.Text -- sent from ServerStat
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

data Heartbeat = Heartbeat !String !UTCTime !ThreadId !Int

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
  , tcServerStat :: MVar (Maybe (IM.IntMap String))
  , tzTokyo :: TimeZoneSeries
  }

-- Runtime enviroment share among threads
type WEnv = (WConf, TCommon)

-- for keeping track of sync-state between twitter thread and telegram thread
data TgSyncState
  = TSPending -- indicate that a tweet is detected but not yet sent to the channel
  | TSSynced Int -- indicate that a tweet is already sent as a telegram message
  | TSIgnored -- tweets acknowledged without syncing to tg (<= tweet-id-greater-than)
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
