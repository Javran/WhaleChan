{-# LANGUAGE
    DeriveGeneric
  , TypeApplications
  , FlexibleContexts
  , LambdaCase
  #-}
module Javran.WhaleChan.ExtInfoThread where

import Data.Time.Clock

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import Data.Aeson
import GHC.Generics
import Network.HTTP.Client
import Data.Default
import Control.Concurrent
import Control.Exception
import Control.Monad.RWS
import Control.Arrow
import Data.List

import Javran.WhaleChan.Types
import Javran.WhaleChan.Base
import Javran.WhaleChan.Util
import qualified Javran.WhaleChan.Log as Log

import qualified Javran.WhaleChan.FromSource.KcsConst as KcsConst
import qualified Javran.WhaleChan.FromSource.Kc3Kai as Kc3Kai
import qualified Javran.WhaleChan.FromSource.Wikia as Wikia
import qualified Javran.WhaleChan.FromSource.Kcwiki as Kcwiki
import qualified Javran.WhaleChan.FromSource.TimeFormat as TFmt

{-
  thread for getting info from external sources
 -}

data ExtInfo
  = ExtInfo
  { maintenanceTimes :: M.Map String (UTCTime, UTCTime)
  , serverInfo :: IM.IntMap String
  } deriving (Eq, Generic)

instance FromJSON ExtInfo
instance ToJSON ExtInfo
instance Default ExtInfo

fakeSource :: (String, Manager -> IO (PRange UTCTime))
fakeSource = ("FakeSourceForTesting", getInfo)
  where
    getInfo _ = pure . pure $ (fakeStart, fakeEnd)
    fakeStart = TFmt.mkUtcInJst 2019 03 07 11 00 00
    fakeEnd = TFmt.mkUtcInJst 2019 03 07 20 00 00

{-
  external sources except KcsConst.

  - as KcsConst also contains server info,
    we want to give it a special treatment
 -}
sources :: [(String, Manager -> IO (PRange UTCTime))]
sources =
    [ ("Kc3Kai", Kc3Kai.getInfo)
    , ("Wikia", Wikia.getInfo)
    , ("Kcwiki", Kcwiki.getInfo)
    -- TODO: this source is for testing only,
    -- remember to remove it when done
    , fakeSource
    ]

type EIM = WCM ExtInfo

pprState :: EIM ()
pprState = do
    let logInfo = Log.i "ExtInfo"
    ExtInfo mt si <- get
    forM_ ("KcsConst" : fmap fst sources) $ \srcName ->
      logInfo $ srcName <> ":\t" <> show (M.lookup srcName mt)
    logInfo $ "serverinfo: " <> show si

extInfoThread :: WEnv -> IO ()
extInfoThread wenv = do
    let (_,TCommon{tcManager=mgr,tcReminder=tRmdr}) = wenv
        logErr = Log.e "ExtInfo"
        consumeErr srcName eResult procResult = case eResult of
            Left e -> logErr $ "Error on source: '" <> srcName <> "', " <> displayException e
            Right v -> procResult v
        extInfoStep markStart = do
            -- pprState
            markEnd <- markStart
            ExtInfo mtOld _ <- get
            -- scan through sources except kcsconst
            forM_ sources $ \(srcName, getInfo) -> do
                result <- liftIO $ guardHttpException (getInfo mgr)
                consumeErr srcName result $ \case
                  Just x ->
                    modify $ \ei@ExtInfo{maintenanceTimes=mts} ->
                      ei {maintenanceTimes = M.insert srcName x mts}
                  Nothing -> pure ()
            -- get data from kcsconst
            do
                let srcName = "KcsConst"
                result <- liftIO $ guardHttpException (KcsConst.getInfo mgr)
                consumeErr srcName result $ \case
                  Just (KcsConst.KcsConst si mt) -> do
                    -- update maintenance info
                    case mt of
                      Just v ->
                        modify $ \ei@ExtInfo{maintenanceTimes=mts} ->
                          ei {maintenanceTimes = M.insert srcName v mts}
                      Nothing -> pure ()
                    -- update server info
                    modify $ \ei -> ei {serverInfo = si}
                  Nothing -> pure ()
            markEnd :: EIM ()
            ExtInfo mtNew _ <- get
            -- TODO: now it always computes
            when (mtNew /= mtOld || True) $ liftIO $ do
                t <- getCurrentTime
                _ <- swapMVar tRmdr (summarize t mtNew)
                pure ()
            {-
              TODO: when difference is detected, we want to send to reminderThread some info
              TODO: should we ignore time in the past?

              (draft) for reminder thread:
              - it should maintain EventReminder that contains some extra info
                about which sources are agreeing on that time.
              - event occur time change (only the closest future time) triggers
                an new reminder while new agreement is updated silently
                (it'll nonetheless show up in future when reminder needs to trigger)
             -}
            liftIO $ threadDelay $ oneSec * 60
    autoWCM @ExtInfo "ExtInfo" "ext-info.yaml" wenv extInfoStep

summarize :: UTCTime -> M.Map String (UTCTime, UTCTime) -> MaintenanceInfo
summarize curTime d =
  if null ps
    then (Nothing, Nothing)
    else let tPStart = (\(src,(l,_)) -> (src,l)) <$> ps
             tPEnd = (\(src,(_,r)) -> (src,r)) <$> ps
         in (getMinWithSrc tPStart, getMinWithSrc tPEnd)
  where
    getMinWithSrc :: [(String,UTCTime)] -> Maybe (UTCTime, [String])
    getMinWithSrc xs = second sort <$> M.lookupMin timeGrps
      where
        timeGrps = M.fromListWith (++) . fmap (\(src,v) -> (v,[src])) $ xs
    ps =
      -- curtime + 10 seconds <= vEnd
      filter (\(_, (_, vEnd)) -> 10 `addUTCTime` curTime <= vEnd)
      . M.toAscList
      $ d
