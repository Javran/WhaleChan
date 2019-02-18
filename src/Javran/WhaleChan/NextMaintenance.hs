{-# LANGUAGE
    NamedFieldPuns
  #-}
module Javran.WhaleChan.NextMaintenance where

import Network.HTTP.Client

import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Javran.WhaleChan.Types

{-
  for figuring out next maintenance time
 -}

{-
  possible sources:

  - game source: http://203.104.209.7/gadget_html5/js/kcs_const.js
  - KC3Kai: https://raw.githubusercontent.com/KC3Kai/KC3Kai/master/update
  - Kcwiki: https://zh.kcwiki.org/wiki/Template:维护倒数
  - Wikia: https://kancolle.fandom.com/wiki/Recent_Updates
 -}

decodeFromRaw :: BSL.ByteString -> Text
decodeFromRaw = decodeUtf8 . BSL.toStrict

getInfoFromGameSource :: WEnv -> IO ()
getInfoFromGameSource (_,TCommon{tcManager}) = do
    req <- parseRequest "http://203.104.209.7/gadget_html5/js/kcs_const.js"
    resp <- httpLbs req tcManager
    print (decodeFromRaw $ responseBody resp)
    pure ()
