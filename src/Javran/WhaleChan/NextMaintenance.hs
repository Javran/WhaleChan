{-# LANGUAGE
    NamedFieldPuns
  #-}
module Javran.WhaleChan.NextMaintenance where

import Network.HTTP.Client

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Text.ParserCombinators.ReadP

import Javran.WhaleChan.Types

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
    putStrLn (decodeFromRaw $ responseBody resp)
    pure ()
