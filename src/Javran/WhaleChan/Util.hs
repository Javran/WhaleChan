{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
module Javran.WhaleChan.Util
  ( describeDuration
  , isYamlFileNotFoundException
  , protectedAction
  ) where

import Data.List (isPrefixOf)
import qualified Data.Yaml as Yaml
import Control.Exception
import Control.Monad
import Say

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

-- run an action forever and keep it running as long as # of critical errors
-- doesn't exceed a limit
protectedAction :: String -> Int -> IO () -> IO ()
protectedAction aName maxRetry action = run 0
  where
    fAction = forever action
    errHandler e =
      sayErrString $ "Exception caught for Action " ++ aName ++ ": " ++ displayException e
    run retryCount
      | retryCount > maxRetry =
          sayErrString $ "Action " ++ aName ++ " exceeded max retry attempt, aborting."
      | otherwise = do
          unless (retryCount == 0) $
            sayErrString $ "At #" ++ show retryCount ++ " reattempt for Action " ++ aName
          catch @SomeException fAction errHandler >> run (retryCount+1)
