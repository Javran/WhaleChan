{-# LANGUAGE
    NamedFieldPuns
  #-}
module Javran.WhaleChan.NextMaintenance where

import Network.HTTP.Client

import Control.Applicative
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Text.ParserCombinators.ReadP
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Either

import Javran.WhaleChan.Types

data KCSConst = KCSConst
  { serverInfo :: IM.IntMap String
  , maintenanceTime :: (String, String)
  } deriving (Show)

{-
  for figuring out next maintenance time
 -}

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

kcsConstFromRaw :: String -> Maybe KCSConst
kcsConstFromRaw = tr . mapMaybe parseLine . lines
  where
    -- all-in-one parser that run through all lines
    allP = (Left <$> serverInfoP)
        <|> (Right . Left <$> mStartTimeP)
        <|> (Right . Right <$> mEndTimeP)
    parseLine inp = case readP_to_S allP inp of
        [(a,"")] -> Just a
        _ -> Nothing
    tr :: [Either (Int, String) (Either String String)] -> Maybe KCSConst
    tr xs = case partitionEithers mTimes of
        ([sT], [eT]) -> Just $ KCSConst (IM.fromList sInfos) (sT,eT)
        _ -> Nothing
      where
        (sInfos, mTimes) = partitionEithers xs
{-
  possible sources:

  - game source: http://203.104.209.7/gadget_html5/js/kcs_const.js
  - KC3Kai: https://raw.githubusercontent.com/KC3Kai/KC3Kai/master/update
  - Kcwiki: https://zh.kcwiki.org/wiki/Template:维护倒数
  - Wikia: https://kancolle.fandom.com/wiki/Recent_Updates
 -}

decodeFromRaw :: BSL.ByteString -> String
decodeFromRaw = T.unpack . decodeUtf8 . BSL.toStrict

getInfoFromGameSource :: WEnv -> IO ()
getInfoFromGameSource (_,TCommon{tcManager}) = do
    req <- parseRequest "http://203.104.209.7/gadget_html5/js/kcs_const.js"
    resp <- httpLbs req tcManager
    print (kcsConstFromRaw $ decodeFromRaw $ responseBody resp)
    pure ()
