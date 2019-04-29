{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , TupleSections
  #-}
module Javran.WhaleChan.ServerStatThread
  ( serverStatThread
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.RWS
import Data.Bifunctor
import Data.Either
import Data.Time.Clock
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T

import Javran.WhaleChan.Types
import Javran.WhaleChan.Util
import Javran.WhaleChan.Base
import qualified Javran.WhaleChan.Log as Log

import Javran.WhaleChan.ServerStatThread.Types
import Javran.WhaleChan.ServerStatThread.Base

{-

  for getting server related infomation.

  - (TODO) in order not to get too noisy, we only post about following events:

    + a new version change is first detected at a specific server
    + announce that versions in all servers are now in sync with latest
    + a server ip is changed
    + a server is down (by trying to get the version number)
    + a server is back online

  - keep a set of VerPack indexed by ascending number.
    detected versions are first compared against this set of items then
    registered if missing (with detection time)

  - we'll try to make minimum of assumption:

    + "gadget" address is always fixed
    + the structure of the version file is always a JSON file
      with component names to version names (both are strings)
    + all servers will eventually agree on a single version data
      (up to JSON level, regardless of key ordering)

  - at a higher level, we don't actually care about whether a specific
    component, when a new change is detected, we put it into a "known version data file"
    list together with a Int referencing to the VerPackDb

  - as soon as a server returns a version data that is not in the "known" map,
    we announce the change (so if other server starts to pick the same new version data file,
    we can remain silence
  - once all servers are caught up, we'll make another announcement
  - to prevent VerPackDb from piling up items that are no longer being used,
    any item no longer being referred by any server will be dropped.

 -}

{-
  we are not using HealthThread for now,
  as the timeout-not-being-respected issue
  only seems to come from twitter-related threads,
 -}
serverStatThread :: WEnv -> IO ()
serverStatThread wenv = do
    {-
      we are creating one dedicated to talking to various kc servers.
      since only this very server talks to these servers,
      it makes sense not to use the shared manager.
     -}
    mgr <- newManager tlsManagerSettings
    autoWCM @State tag "server-stat.yaml" wenv (threadStep mgr)

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

{-
  - scan all servers and download VerPack for inspection
  - update sKcServerStates accordingly
 -}
scanAllServers :: Manager -> M ()
scanAllServers mgr = do
  State {sServerAddrs = as} <- get
  {-
    start async actions to fetch VerPack from all known servers
    and wait for their completions.
   -}
  aResults <- liftIO $ do
    aActions <- traverse (async . getInfoFromKcServer mgr) as
    traverse waitCatch aActions
  let errs :: [(Int, SomeException)]
      results :: [(Int, (VerPack, UTCTime))]
      (errs, results) =
          partitionEithers
          . fmap (\(k,e) -> bimap (k,) (k,) e)
          . IM.toList
          $ aResults
      errCount = length errs
      resCount = length results
  when (errCount > 0 || resCount /= IM.size as) $
    Log.i tag $ "abnormal: (# of errors, # of success)=" <> show (errCount, resCount)
  when (errCount > 0 && IM.size as == errCount) $ do
    Log.w tag "encountering error from all servers"
    bNetwork <- liftIO checkNetwork
    if bNetwork
      then
        Log.w tag "looks like all servers are down"
      else
        Log.w tag "looks like network is down"
  -- now another traversal to update the State of current thread for each server.
  forM_ (IM.toList aResults) $ \(serverId, aResult) -> case aResult of
    Left e ->
      Log.e tag $
        T.unpack (describeServer serverId)
        <> " encountered exception: "
        <> displayExceptionShort e
    Right (vp, t) -> do
      vpId <- registerVerPack vp
      modify $ \s ->
        let kss = sKcServerStates s
            v = KcServerState vpId t
        in s {sKcServerStates = IM.insert serverId v kss}

{-
  scan through sKcServerStates and drop sVerPackDb items no longer being referred
 -}
cleanupDb :: M ()
cleanupDb = do
  refs <- fmap ssVerPackKey . IM.elems <$> gets sKcServerStates
  db <- gets sVerPackDb
  let (dbKeep, dbDrop) = IM.partitionWithKey (\k _ -> k `elem` refs) db
  unless (IM.null dbDrop) $
    Log.i tag $ "Dropping: " <> show dbDrop
  modify (\s -> s {sVerPackDb = dbKeep})

threadStep :: Manager -> M (M ()) -> M ()
threadStep mgr markStart = do
    (_,TCommon{tcServerStat=ch}) <- ask
    markEnd <- markStart
    mServerInfo <- liftIO $ swapMVar ch Nothing
    -- update server addrs (if available)
    case mServerInfo of
      Just si -> do
        -- TODO: warn about potentially server outage?
        {-
          here we are replacing server addrs with
          whatever info available from channel rather than updating
          as the infomation is parsed from same file
          therefore they should stay together
         -}
        oldSi <- gets sServerAddrs
        modify $ \s -> s {sServerAddrs = si}
        let sDiff = mapDiff oldSi si
        case renderServerAddrDiffMd sDiff of
          Nothing -> pure ()
          Just content -> writeToTg (TgRMServerStat content)
      Nothing -> pure ()
    dbBefore <- gets sVerPackDb
    scanAllServers mgr
    dbAfter <- gets sVerPackDb
    when (dbBefore /= dbAfter) $ do
      Log.i tag "Found difference in VerPackDb"
      {-
        note #1: up until this point we have removed nothing
        from db, meaning that if dbBefore is non-empty, so will dbAfter be.

        note #2: hopefully VerPack with the lowest vpId is the one that
        all servers have agreed upon (in the past) and the max one after
        server scan is the latest version of VerPack, comparing these two
        allows us to derive detailed info about which part of the game has been updated.
       -}
      case (IM.minView dbBefore, IM.maxView dbAfter) of
        (Nothing, _) -> Log.i tag "Fresh start."
        (Just (vpBefore, _), Just (vpAfter, _)) ->
          when (IM.size dbBefore == 1) $ do
            let vpd = vpBefore `mapDiff` vpAfter
                tgMessage = renderVerPackDiffMd vpd
            Log.i tag $ "DiffResult: " <> show vpd
            {-
              post new message when a new VerPack is known
             -}
            writeToTg (TgRMServerStat tgMessage)
        _ -> do
          -- this should be unreachable
          Log.e tag "Unreachable code path reached?"
          Log.e tag $ "Before & After: " <> show (dbBefore, dbAfter)

    cleanupDb
    dbAfterClean <- gets sVerPackDb
    when (dbAfter /= dbAfterClean && IM.size dbAfterClean == 1) $ do
      let tgMessage =
            simpleMarkdownEscape "[ServerStat] New game version observed on all servers"
      writeToTg (TgRMServerStat tgMessage)
    markEnd
    {-
      wake up every 53 seconds.
      the number is a random choice to be close to 1 minutes,
      picking a prime number to reduce the amount simultaneous events.
     -}
    liftIO $ threadDelay $ oneSec * 53
