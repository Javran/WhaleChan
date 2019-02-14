{-# LANGUAGE
    LambdaCase
  , ExplicitForAll
  , ScopedTypeVariables
  , TypeApplications
  #-}
module Javran.WhaleChan.Base
  ( loadWEnv
  , autoWCM
  ) where

import qualified Data.Yaml as Yaml
import Control.Monad
import Control.Exception
import Control.Monad.RWS
import Data.Default
import Say

import Javran.WhaleChan.Types
import Javran.WhaleChan.Util

{-
  it is assumed that all files related to the current
  running instance is located in current directory.
  the main program will accept a path to that directory
  and switch immediately to it at startup.
  all proceeding operations on files should just use a predefined name
  without using subdirectories.

  - config.yaml: config file

 -}

loadWEnv :: IO WConf
loadWEnv = Yaml.decodeFileThrow "config.yaml"

{-
  WCM thread template:
  - every thread loads its own state file at startup
  - then initiate an infinte loop
      + during each loop some amount of work will be performed (i.e. step)
      + and the thread is intercepted at beginning and ending of a loop
        to write state to file if the state differs
  - the thread is protected against SomeException for atmost 16 times.
    in other words, we tolerate at most 16 critical exception
 -}
autoWCM ::
    forall s. (Eq s, Yaml.FromJSON s, Yaml.ToJSON s, Default s)
    => String -> FilePath -> WEnv
    -> (WCM s (WCM s ()) -> WCM s ()) -> IO ()
autoWCM mName stateFp wenv step =
    protectedAction mName 16 $
        Yaml.decodeFileEither @s stateFp >>= \case
            Left e -> do
                unless (isYamlFileNotFoundException e) $ do
                  sayErrString $
                    "Exception caught while loading state file for " ++ mName
                    ++ ": " ++ displayException e
                  sayErrString $ "Will use default state for " ++ mName
                run def
            Right st -> run st
  where
    run st = void (evalRWST (forever m) wenv st)
    m =
      let markStart :: WCM s (WCM s ())
          markStart = do
            liftIO $ putStrLn "marking start"
            oldSt <- get
            pure $ do
              liftIO $ putStrLn "marking end"
              newSt <- get
              unless (oldSt == newSt) $
                liftIO (Yaml.encodeFile stateFp newSt)
      in void $ step markStart
