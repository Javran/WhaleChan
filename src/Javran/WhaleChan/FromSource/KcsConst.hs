{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , TypeApplications
  #-}
module Javran.WhaleChan.FromSource.KcsConst where

import Control.Applicative
import Data.Either
import Data.Maybe
import Data.Time.Clock
import Text.ParserCombinators.ReadP

import qualified Data.IntMap as IM
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Data.Time.LocalTime

import Javran.WhaleChan.Types
import Javran.WhaleChan.Util
import Javran.WhaleChan.FromSource.Util
import Javran.WhaleChan.FromSource.TimeFormat

mTimeFieldP :: String -> ReadP String
mTimeFieldP fd =
    string fd *> skipSpaces *> string "=" *> skipSpaces
    *> string "Date.parse(\"" *> munch1 (/='\"') <* string "\");"
    <* skipSpaces <* eof

mStartTimeP, mEndTimeP :: ReadP String
mStartTimeP = mTimeFieldP "MaintenanceInfo.StartDateTime"
mEndTimeP = mTimeFieldP "MaintenanceInfo.EndDateTime"

serverInfoP :: ReadP (Int, String)
serverInfoP =
    (,) <$> ( string "ConstServerInfo.World_"
              *> readS_to_P reads
              <* skipSpaces <* string "=" <* skipSpaces
            )
        <*> ( string "\""
              *> munch1 (/='\"')
              <* string "\";" <* skipSpaces <* eof
            )

data KcsConst a = KcsConst
  { serverInfo :: IM.IntMap String
  , maintenanceTime :: a
  } deriving (Show)

kcsConstFromRaw :: String -> Maybe (KcsConst (String, String))
kcsConstFromRaw = tr . mapMaybe parseLine . lines
  where
    -- all-in-one parser that run through all lines
    allP = (Left <$> serverInfoP)
        <|> (Right . Left <$> mStartTimeP)
        <|> (Right . Right <$> mEndTimeP)
    parseLine inp = case readP_to_S allP inp of
        [(a,"")] -> Just a
        _ -> Nothing
    tr xs = case partitionEithers mTimes of
        ([sT], [eT]) -> Just $ KcsConst (IM.fromList sInfos) (sT,eT)
        _ -> Nothing
      where
        (sInfos, mTimes) = partitionEithers xs

decodeFromRaw :: BSL.ByteString -> String
decodeFromRaw = T.unpack . decodeUtf8 . BSL.toStrict

fmtKcsConst :: String
fmtKcsConst = "%Y/%m/%d %T"

parseTime :: String -> Maybe UTCTime
parseTime raw = do
    t <- eitherToMaybe $ mkTimeParser @LocalTime fmtKcsConst raw
    pure $ zonedTimeToUTC (ZonedTime t jst)

getInfo :: Manager -> IO (Maybe (KcsConst (Maybe (PRange UTCTime))))
getInfo mgr = do
    content <- fetchUrl mgr "http://203.104.209.7/gadget_html5/js/kcs_const.js"
    case kcsConstFromRaw (decodeFromRaw content) of
      Nothing -> pure Nothing
      Just (KcsConst si (l,r)) -> do
        let l' = parseTime l
            r' = parseTime r
        pure $ Just (KcsConst si (toPRange l' r'))
