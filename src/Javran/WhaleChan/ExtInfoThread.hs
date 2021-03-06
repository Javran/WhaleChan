{-# LANGUAGE
    DeriveGeneric
  , TypeApplications
  , FlexibleContexts
  , LambdaCase
  #-}
module Javran.WhaleChan.ExtInfoThread
  ( extInfoThread
  , fakeSource
  ) where

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad.RWS
import Data.Aeson
import Data.Default
import Data.List
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Client

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

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

  the info being held is described by type ExtInfo
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
    fakeStart = TFmt.mkUtcInJst 2019 03 15 11 00 00
    fakeEnd = TFmt.mkUtcInJst 2019 03 15 20 00 00

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
    ]

type EIM = WCM ExtInfo

extInfoThread :: WEnv -> IO ()
extInfoThread wenv = do
    let (_,TCommon{tcManager=mgr,tcReminder=tRmdr,tcServerStat=tSS}) = wenv
        logErr = Log.e "ExtInfo"
        consumeErr srcName eResult procResult = case eResult of
            Left e ->
              logErr $
                "Error on source: '" <> srcName <> "', "
                <> displayExceptionShort (toException e)
            Right v -> procResult v
        extInfoStep markStart = do
            markEnd <- markStart
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
            ExtInfo mtNew si <- get
            {-
              always send message to interested threads.
              the idea is to allow recipient's persistent state to be wiped out
              (e.g. reminder state being removed) without having to wipe out
              ExtInfo's own persistent state.
             -}
            liftIO $ do
                t <- getCurrentTime
                -- put maintenance info to ReminderThread
                _ <- swapMVar tRmdr (summarize t mtNew)
                -- put server map to ServerStatThread
                _ <- swapMVar tSS (Just si)
                threadDelay $ oneSec * 60
    autoWCM @ExtInfo "ExtInfo" "ext-info.yaml" wenv extInfoStep

{-
  given the current time, and a bunch of start & end time together
  with name of the source attached,
  compute the MaintenanceInfo that we agree on.
 -}
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
    {-
      remove old maintenance info by removing those whose end time
      is in the past
     -}
    ps =
      -- curtime + 10 seconds <= vEnd
      filter (\(_, (_, vEnd)) -> 10 `addUTCTime` curTime <= vEnd)
      . M.toAscList
      $ d
