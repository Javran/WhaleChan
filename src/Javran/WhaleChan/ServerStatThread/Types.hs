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
  , VerPackDiff
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
  { ssVerPackKey :: Int
  , ssLastContact :: UTCTime
  } deriving (Eq, Generic)

instance FromJSON KcServerState
instance ToJSON KcServerState

data State
  = State
  { sServerAddrs :: IM.IntMap String -- value example: "http://203.104.209.71/"
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
type VerPackDiff = MapDiffResult M.Map T.Text T.Text
