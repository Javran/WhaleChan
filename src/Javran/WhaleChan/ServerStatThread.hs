{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , DeriveGeneric
  #-}
module Javran.WhaleChan.ServerStatThread
  ( serverStatThread
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.RWS
import Data.Aeson
import Data.Default
import Data.Either
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Client

import Javran.WhaleChan.Types
import Javran.WhaleChan.Util
import Javran.WhaleChan.Base
import qualified Javran.WhaleChan.Log as Log

{-
  for getting server related infomation.
 -}

{-
  design draft:
  (TODO) impl

  - in order not to get too noisy, we only post about following events:

    + a new version change is first detected at a specific server
    + announce that versions in all servers are now in sync with latest
    + a server ip is changed
    + a server is down (by trying to get the version number)
    + a server is back online

  - keep a list of [(UTCTime, VersionInfo)] in descending order of time,
    detected versions are first compared against this list then
    registered if missing (with detection time)

  - we'll try to make minimum of assumption:

    + "gadget" address is always fixed
    + the structure of the version file is always a JSON file
      with component names to version names (both are strings)
    + all servers will eventually agree on a single version data
      (up to JSON level, regardless of key ordering)

  - at a higher level, we don't actually care about whether a specific
    component, when a new change is detected, we put it into a "known version data file"
    list together with a timestamp
  - as soon as a server returns a version data that is not in the "known" map,
    we announce the change (so if other server starts to pick the same new version data file,
    we can remain silence
  - once all servers are caught up, we'll make another announcement
  - optimize: remove item from known version data file once the item in question
    is no longer being used by any server

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
  - (TODO) in the future, no longer referred data will be removed as well.
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

-- known server names
_serverNamesTable :: IM.IntMap T.Text
_serverNamesTable = IM.fromList
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

serverStatThread :: WEnv -> IO ()
serverStatThread wenv =
    autoWCM @State tag "server-stat.yaml" wenv threadStep

tag :: String
tag = "ServerStat"

type M = WCM State

-- try to download resource from a kcserver
getInfoFromKcServer :: Manager -> String -> IO (VerPack, UTCTime)
getInfoFromKcServer mgr addr = do
    let url = addr <> "/kcs2/version.json"
    req <- parseUrlThrow url
    raw <- responseBody <$> httpLbs req mgr
    let Just vp = decode raw
    t <- getCurrentTime
    pure (vp, t)

registerVerPack :: VerPack -> M Int
registerVerPack vp = do
    State {sVerPackDb = db} <- get
    let vps = IM.toList db
        -- admittedly this is not an efficient way to do it
        -- but in our case the map is very small.
        existing = filter ((==vp) . snd) vps
    case existing of
      [] -> do
        let thisKey =
              if IM.null db
                then 0
                else fst (IM.findMax db) + 1
        modify (\s -> s {sVerPackDb = IM.insert thisKey vp db })
        pure thisKey
      [(k, _)] -> pure k
      _ -> error "uncreachable"

scanAllServers :: M ()
scanAllServers = do
  -- TODO: should we use a dedicated manager instead?
  (_,TCommon{tcManager=mgr}) <- ask
  State {sServerAddrs = as} <- get
  aResults <- liftIO $ do
    aActions <- traverse (async . getInfoFromKcServer mgr) as
    traverse waitCatch aActions
  let (errs, results) =
          partitionEithers
          . fmap (\(k,e) -> case e of -- TODO: should be better ways of writing this.
                     Left l -> Left (k,l)
                     Right r -> Right (k,r)
                     )
          . IM.toList
          $ aResults
      errCount = length errs
      resCount = length results
  when (errCount > 0) $
    Log.i tag $ "error count = " <> show (length errs)
  when (resCount /= IM.size as) $
    Log.i tag $ "result count = " <> show (length results)
  -- TODO for now we do "dark register", which silently registers but tells nothing
  vpIds <- mapM (\(_k, (vp, _t)) -> registerVerPack vp) results
  case vpIds of
    [] -> Log.i tag "no vp id available"
    [_] -> Log.i tag "exactly one vp id found"
    x:xs ->
      when (any (/=x) xs) $
        Log.i tag $ "VerPackIds: " <> show vpIds

threadStep :: M (M ()) -> M ()
threadStep markStart = do
    (_,TCommon{tcServerStat=ch}) <- ask
    markEnd <- markStart
    mServerInfo <- liftIO $ swapMVar ch Nothing
    -- update server addrs (if available)
    case mServerInfo of
      Just si ->
        -- TODO: signal changes
        {-
          here we are replacing server addrs with
          whatever info available from channel rather than updating
          as the infomation is parsed from same file
          therefore they should stay together
         -}
        modify $ \s-> s {sServerAddrs = si}
      Nothing ->
        pure ()
    scanAllServers
    {-
      TODO:
      - scan servers and download VerPack for inspection
      - update sKcServerStates accordingly
      - post new message when:
        + a new VerPack is known
        + all servers are caught up on VerPack
     -}
    markEnd
    -- wake up every hour, it's not doing anything anyway.
    liftIO $ threadDelay $ oneSec * 60 * 60
    -- TODO: much shorter sleep just for debugging.
    -- liftIO $ threadDelay oneSec
