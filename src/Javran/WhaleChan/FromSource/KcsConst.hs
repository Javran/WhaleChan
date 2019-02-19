{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  #-}
module Javran.WhaleChan.FromSource.KcsConst where

import Control.Applicative
import Data.Either
import Data.Maybe
import Text.ParserCombinators.ReadP

import qualified Data.IntMap as IM

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

data KCSConst = KCSConst
  { serverInfo :: IM.IntMap String
  , maintenanceTime :: (String, String)
  } deriving (Show)

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
