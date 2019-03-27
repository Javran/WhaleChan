{-# LANGUAGE
    OverloadedStrings
  #-}
module Javran.WhaleChan.ServerStatThread where

import Data.Time.Clock

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T

{-
  for getting server related infomation.
 -}

{-
  design draft:

  - in order not to get too noisy, we only post about following events:

    + a new version change is first detected at a specific server
    + announce that versions in all servers are now in sync with latest
    + a server ip is changed
    + a server is down (by trying to get the version number)
    + a server is back online

  - keep a list of [(UTCTime, VersionInfo)] in descending order of time,
    detected versions are first compared against this list then
    registered if missing (with detection time)

 -}
data ServerState
  = ServerState
  { ssVersionTimestamp :: UTCTime
  , ssLastContact :: UTCTime
  }

data Version
  = Version
  { verRaw :: String
  , verGroups :: [Either String Integer]
  }

type VersionInfoCache =
  [(UTCTime, M.Map T.Text Version)] -- in descending order of time

data State
  = State
  { sServerIps :: IM.IntMap String -- value example: "203.104.209.71"
  , sServerState :: IM.IntMap ServerState
  , sVersionInfoCache :: VersionInfoCache
  }

-- known server names
serverNamesTable :: IM.IntMap T.Text
serverNamesTable = IM.fromList
  [ (1 , "横須賀鎮守府")
  , (2 , "呉鎮守府")
  , (3 , "佐世保鎮守府")
  , (4 , "舞鶴鎮守府")
  , (5 , "大湊警備府")
  , (6 , "トラック泊地")
  , (7 , "リンガ泊地")
  , (8 , "ラバウル基地")
  , (9 , "ショートランド泊地")
  , (10 , "ブイン基地")
  , (11 , "タウイタウイ泊地")
  , (12 , "パラオ泊地")
  , (13 , "ブルネイ泊地")
  , (14 , "単冠湾泊地")
  , (15 , "幌筵泊地")
  , (16 , "宿毛湾泊地")
  , (17 , "鹿屋基地")
  , (18 , "岩川基地")
  , (19 , "佐伯湾泊地")
  , (20 , "柱島泊地")
  ]
