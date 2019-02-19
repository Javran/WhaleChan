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
import Javran.WhaleChan.FromSource.Kc3Kai
import Javran.WhaleChan.FromSource.Wikia
import Javran.WhaleChan.FromSource.Kcwiki

{-
  this module is for figuring out next maintenance time from various sources

  possible sources are:

  - game source: http://203.104.209.7/gadget_html5/js/kcs_const.js
  - Kc3Kai: https://raw.githubusercontent.com/KC3Kai/KC3Kai/master/update
  - Kcwiki: https://zh.kcwiki.org/wiki/Template:维护倒数
  - Wikia: https://kancolle.fandom.com/wiki/Recent_Updates

  notes:

  - Date.parse spec as impl in v8: https://github.com/v8/v8/blob/8bb236d7c91cc1cbc5ddb656b57bcaa51eeb5b54/src/dateparser-inl.h#L24-L70
    too complicated to impl for our case.

  - data sample:

```
KcsConst
  Just ("2019/02/08 11:00:00","2019/02/08 20:25:00")
Kc3Kai
  KC3TimeRaw start: Fri, 08 February 2019 11:00:00 +0900, end: Fri, 08 February 2019 21:00:00 +0900
Wikia
  ((Just "February 27 2019 11:00:00 +0900",Just "February 27 2019 19:00:00 +0900"),[])
Kcwiki
  (Just "2019/2/27 11:00:00 +0900",[])
```

  - potential impl: date parser + time parser + timezone parser (missing -> maybe)
    + date parser: "YYYY/M[M]/D[D]" or "[Month name] day year" or "day [Month name] year"
    + time parser: "HH:MM:SS"
    + timezone: "[+-]0000"

  - steps:
    + construct as Date.Time.LocalTime, convert to UTCTime with UTC timezone
    + timezone is then added as offset
    + these steps are to avoid `timeZoneSummerOnly` and `timeZoneName`
  - alternatively http://hackage.haskell.org/package/time-1.8.0.2/docs/Data-Time-Format.html could be of interest

 -}

decodeFromRaw :: BSL.ByteString -> String
decodeFromRaw = T.unpack . decodeUtf8 . BSL.toStrict

getInfoFromGameSource :: WEnv -> IO ()
getInfoFromGameSource (_,TCommon{tcManager}) = do
    req <- parseRequest "http://203.104.209.7/gadget_html5/js/kcs_const.js"
    resp <- httpLbs req tcManager
    print ((maintenanceTime <$>) $ kcsConstFromRaw $ decodeFromRaw $ responseBody resp)
    pure ()

getInfoFromKc3Kai :: WEnv -> IO ()
getInfoFromKc3Kai (_,TCommon{tcManager}) = do
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
    print (parseMaintenanceTime (responseBody resp))

getInfoFromKcwiki :: WEnv -> IO ()
getInfoFromKcwiki (_,TCommon{tcManager}) = do
    req <- parseRequest "https://zh.kcwiki.org/wiki/Template:维护倒数?action=render"
    resp <- httpLbs req tcManager
    print (parseKcwikiMaintenanceStartTime (responseBody resp))

sourceTest :: WEnv -> IO ()
sourceTest e = do
    putStrLn "KcsConst"
    putStr "  " >> getInfoFromGameSource e
    putStrLn "Kc3Kai"
    putStr "  " >> getInfoFromKc3Kai e
    putStrLn "Wikia"
    putStr "  " >> getInfoFromWikia e
    putStrLn "Kcwiki"
    putStr "  " >> getInfoFromKcwiki e
