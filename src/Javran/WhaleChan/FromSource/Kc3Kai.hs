{-# LANGUAGE
    OverloadedStrings
  , DataKinds
  , TypeFamilies
  #-}
module Javran.WhaleChan.FromSource.Kc3Kai where

import Prelude hiding (fail)
import Control.Monad.Fail
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Time.Clock

import Javran.WhaleChan.Types
import Javran.WhaleChan.FromSource.TimeFormat
import Javran.WhaleChan.FromSource.Util

data KC3Time = KC3Time UTCTime UTCTime

instance FromJSON KC3Time where
  parseJSON = withObject "KC3Time" $ \o ->
      KC3Time <$> (o .: "maintenance_start" >>= parseTime)
              <*> (o .: "maintenance_end" >>= parseTime)

fmtStr :: String
fmtStr = "%a, %d %B %Y %T %z"

parseTime :: String -> Parser UTCTime
parseTime raw = case mkTimeParser fmtStr raw of
  Left err -> fail err
  Right x -> pure x

getInfo :: Manager -> IO (Maybe (PRange UTCTime))
getInfo mgr = do
    content <- fetchUrl mgr "https://raw.githubusercontent.com/KC3Kai/KC3Kai/master/update"
    case eitherDecode content of
      Left e -> do
        putStrLn $ "parse error: " ++ show e
        pure Nothing
      Right (KC3Time sTime eTime) ->
        pure $ toPRange (Just sTime) (Just eTime)
