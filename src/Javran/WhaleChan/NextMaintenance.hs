{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  #-}
module Javran.WhaleChan.NextMaintenance where

import Network.HTTP.Client

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson

import Javran.WhaleChan.Types
import Javran.WhaleChan.FromSource.KcsConst

{-
  for figuring out next maintenance time
 -}

{-
  possible sources:

  - [x] game source: http://203.104.209.7/gadget_html5/js/kcs_const.js
  - [ ] KC3Kai: https://raw.githubusercontent.com/KC3Kai/KC3Kai/master/update
  - [ ] Kcwiki: https://zh.kcwiki.org/wiki/Template:维护倒数
  - [ ] Wikia: https://kancolle.fandom.com/wiki/Recent_Updates
 -}

decodeFromRaw :: BSL.ByteString -> String
decodeFromRaw = T.unpack . decodeUtf8 . BSL.toStrict

getInfoFromGameSource :: WEnv -> IO ()
getInfoFromGameSource (_,TCommon{tcManager}) = do
    req <- parseRequest "http://203.104.209.7/gadget_html5/js/kcs_const.js"
    resp <- httpLbs req tcManager
    print (kcsConstFromRaw $ decodeFromRaw $ responseBody resp)
    pure ()

data KC3TimeRaw = KC3TimeRaw String String

instance FromJSON KC3TimeRaw where
  parseJSON = withObject "KC3TimeRaw" $ \o ->
      KC3TimeRaw <$> o .: "maintenance_start"
                 <*> o .: "maintenance_end"

getInfoFromKC3Kai :: WEnv -> IO ()
getInfoFromKC3Kai (_,TCommon{tcManager}) = do
    req <- parseRequest "https://raw.githubusercontent.com/KC3Kai/KC3Kai/master/update"
    resp <- httpLbs req tcManager
    case eitherDecode (responseBody resp) of
      Left e -> putStrLn $ "parse error: " ++ show e
      Right (KC3TimeRaw sTime eTime) ->
        putStrLn $ "KC3TimeRaw" ++ " start: " ++ sTime ++ ", end: " ++ eTime

getInfoFromWikia :: WEnv -> IO ()
getInfoFromWikia (_,TCommon{tcManager}) = do
    req <- parseRequest "https://kancolle.fandom.com/wiki/Recent_Updates?action=render"
    resp <- httpLbs req tcManager
    pure ()
