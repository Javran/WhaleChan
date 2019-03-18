{-# LANGUAGE
    OverloadedStrings
  #-}
module Javran.WhaleChan.Util
  ( oneSec
  , oneMin
  , describeDuration
  , isYamlFileNotFoundException
  , eitherToMaybe
  , guardHttpException
  , isConnectionResetException
  ) where

import Control.Exception
import Data.List (isPrefixOf)
import qualified Data.Yaml as Yaml
import GHC.IO.Exception
import Network.HTTP.Client

{-
  place for some commonly used functions.
  functions here should not require Javran.WhaleChan.* to work
 -}

oneSec :: Int
oneSec = 1000000

oneMin :: Int
oneMin = oneSec * 60

describeDuration :: Int -> String
describeDuration seconds
  | seconds == 1 = "1 second"
  | seconds < 60 = show seconds <> " seconds"
  | otherwise =
    let (dd, hh') = seconds `divMod` 86400
        (hh, ss') = hh' `divMod` 3600
        (mm, ss) = ss' `divMod` 60
        ddStr = [ if dd == 1 then "1 day" else show dd <> " days" | dd > 0 ]
        hhStr = [ if hh == 1 then "1 hour" else show hh <> " hours" | hh > 0 ]
        mmStr = [ if mm == 1 then "1 minute" else show mm <> " minutes" | mm > 0 ]
        ssStr = [ if ss == 1 then "1 second" else show ss <> " seconds" | ss > 0 ]
    in unwords $ concat [ddStr, hhStr, mmStr, ssStr]

-- stolen from:
-- https://github.com/snoyberg/yaml/blob/35f0286d83acf6c27e00cf8edbfc43c841109760/yaml/test/Data/Yaml/IncludeSpec.hs#L131-L134
isYamlFileNotFoundException :: Yaml.ParseException -> Bool
isYamlFileNotFoundException (Yaml.InvalidYaml (Just (Yaml.YamlException msg)))
  | "Yaml file not found: " `isPrefixOf` msg = True
isYamlFileNotFoundException _ = False

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

-- for guarding against http-client related exceptions
guardHttpException :: IO a -> IO (Either HttpException a)
guardHttpException action =
  (Right <$> action) `catch` (pure . Left)

isConnectionResetException :: IOException -> Bool
isConnectionResetException ioe
  | IOError
    { ioe_type = ResourceVanished
    , ioe_description = "Connection reset by peer"
    } <- ioe = True
  | otherwise = False
