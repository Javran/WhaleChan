{-# LANGUAGE
    OverloadedStrings
  #-}
module Javran.WhaleChan.Util
  ( describeDuration
  , isYamlFileNotFoundException
  , eitherToMaybe
  , guardHttpException
  ) where

import Data.List (isPrefixOf)
import qualified Data.Yaml as Yaml
import Network.HTTP.Client
import Control.Exception

{-
  place for some commonly used functions.
  functions here should not require Javran.WhaleChan.* to work
 -}

describeDuration :: Int -> String
describeDuration seconds
  | seconds == 1 = "1 second"
  | seconds < 60 = show seconds <> " seconds"
  | otherwise =
    let (hh,ss') = seconds `divMod` 3600
        (mm,ss) = ss' `divMod` 60
        hhStr = [ if hh == 1 then "1 hour" else show hh <> " hours" | hh > 0 ]
        mmStr = [ if mm == 1 then "1 minute" else show mm <> " minutes" | mm > 0 ]
        ssStr = [ if ss == 1 then "1 second" else show ss <> " seconds" | ss > 0 ]
    in unwords $ concat [hhStr, mmStr, ssStr]

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
