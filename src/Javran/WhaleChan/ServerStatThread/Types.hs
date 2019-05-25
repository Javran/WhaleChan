{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  #-}
module Javran.WhaleChan.ServerStatThread.Types
  ( VerPack
  , VerPackDb
  , KcServerState(..)
  -- following exports are ServerStatThread-internals:
  , State(..), M
  , tag
  , MapDiffResult
  ) where

import Data.Aeson
import Data.Default
import Data.Time.Clock
import GHC.Generics

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Javran.WhaleChan.Types

{-
  Type definitions for working with ServerStatThread
 -}

{-
  a VerPack stands for content parsed from kcs2/version.json,
  meaning its a set of pairs from components to their corresponding version strings.
 -}
type VerPack = M.Map T.Text T.Text

{-
  contains all known VerPack data
  - any unknown VerPack is registered here with an incremental key.
    (i.e. new key = current maximum key + 1)
  - no longer referred data will be removed as well.
 -}
type VerPackDb = IM.IntMap VerPack

data KcServerState
  = KcServerState
  { -- TODO: use Maybe in case we have working network but the data cannot be parsed.
    ssVerPackKey :: Int
  , ssLastContact :: UTCTime
  } deriving (Eq, Generic)

{-
  TODO: we'll change KcServerState to the following fields:

  - ssVerPackKey :: Int, we'll still use the old structure instead of Maybe,
    but use `-1` to indicate nothing (a failure in parsing)
  - ssLastContact :: UTCTime, last successful contact
  - ssLastWarningTime :: UTCTime, last time that we send a warning of server being down.
    the idea is not to spam the channel too much when a server is down for several hours,
    let's only send warning for a minimum interval of 20 minutes.
    (arbitrary decision, let's see how this will play out.)

 -}

instance FromJSON KcServerState
instance ToJSON KcServerState

data State
  = State
  { -- note that sServerAddrs is the source of truth when answering
    -- about the total number of servers.
    -- value example: "http://203.104.209.71/"
    sServerAddrs :: IM.IntMap String
  , sKcServerStates :: IM.IntMap KcServerState
  , sVerPackDb :: VerPackDb
  } deriving (Eq, Generic)

instance FromJSON State
instance ToJSON State
instance Default State

tag :: String
tag = "ServerStat"

type M = WCM State

type MapDiffResult m k v = ((m k v, m k v), m k (v,v))
