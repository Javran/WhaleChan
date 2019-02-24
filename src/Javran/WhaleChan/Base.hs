{-# LANGUAGE
    LambdaCase
  , ExplicitForAll
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , NamedFieldPuns
  , OverloadedStrings
  #-}
module Javran.WhaleChan.Base
  ( loadWEnv
  , autoWCM
  , logCurrentMessage
  , startLogger
  , wenvToLoggerIO
  ) where

import Control.Monad
import Control.Monad.Logger
import Control.Exception
import Control.Monad.RWS
import Data.Default
import Control.Concurrent

import Javran.WhaleChan.Types
import qualified Javran.WhaleChan.Log as Log
import Javran.WhaleChan.Util
import Data.Time.Clock
import Data.Time.Format
import Data.String
import System.IO
import System.Log.FastLogger

import qualified Data.ByteString as BS
import qualified Data.Yaml as Yaml
import System.Console.Terminfo.Base
import System.Console.Terminfo.Color
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T

type LoggerIO = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

{-
  it is assumed that all files related to the current
  running instance is located in current directory.
  the main program will accept a path to that directory
  and switch immediately to it at startup.
  all proceeding operations on files should just use a predefined name
  without using subdirectories.

  - config.yaml: config file
  - reminder.yaml: for the reminder thread
  - tweet-sync.yaml: for the tweet sync thread

 -}

loadWEnv :: IO WConf
loadWEnv = Yaml.decodeFileThrow "config.yaml"

{-
  WCM thread template:
  - every thread loads its own state file at startup
  - then initiate an infinte loop
      + during each loop some amount of work will be performed (i.e. step)
      + and the thread is (optionally) intercepted to write state to file if the state differs
        at begining and end of a "critical section" (see what this means below)
  - the thread is protected against SomeException for at most 16 times.
    in other words, we tolerate at most 16 critical exception
  - the "step" function accepts a "markStart" action, whom together with
    its return value marks the "critical section" of current step
    (by critical section I mean parts that can get the state modified)

    example impl of `step`:

    step markStart = do
        <... some stuff that doesn't change state ...>
        markEnd <- markStart
        <... some stuff that do change state ...>
        markEnd
        <... some other stuff like thread sleep ...>
  - note that the use of markStart is optional if step has other means
    of implementing state serialization.
    but whenever markStart is called, exactly one markEnd should follow.
    (also it's expected that markStart is called no more than once during current step)
 -}
autoWCM ::
    forall s m. (Eq s, Yaml.FromJSON s, Yaml.ToJSON s, Default s, m ~ WCM s)
    => String -> FilePath -> WEnv
    -> (m (m ()) -> m ()) -> IO ()
autoWCM mName stateFp wenv step =
    protectedAction loggerIO mName 16 $
        Yaml.decodeFileEither @s stateFp >>= \case
            Left e -> do
                unless (isYamlFileNotFoundException e) $ do
                  logErr $
                    "Exception caught while loading state file for " ++ mName
                    ++ ": " ++ displayException e
                  logErr $ "Will use default state for " ++ mName
                run def
            Right st -> run st
  where
    loggerIO = wenvToLoggerIO wenv
    logErr = Log.e' loggerIO mName
    run st = void (evalRWST (runLoggingT (forever m) loggerIO) wenv st)
    m = void $ step markStart
      where
        markStart :: m (m ())
        markStart = do
            oldSt <- get
            pure $ do
                newSt <- get
                unless (oldSt == newSt) $
                  liftIO (Yaml.encodeFile stateFp newSt)

logLevelToLogStr :: LogLevel -> LogStr
logLevelToLogStr = \case
    LevelDebug -> "D"
    LevelInfo -> "I"
    LevelWarn -> "W"
    LevelError -> "E"
    LevelOther {} -> "?"

utcTimeToLogStr :: UTCTime -> LogStr
utcTimeToLogStr = fromString . formatTime defaultTimeLocale fmtStr
  where
    fmtStr = iso8601DateFormat (Just "%T%04Q")

-- as long as we know the channel, it'll be possible to route logs to
-- the dedicated logger thread
logCurrentMessage :: Chan WLog -> LoggerIO
logCurrentMessage ch _ _ lvl msg = do
    t <- getCurrentTime
    writeChan ch (WLog t lvl msg)

-- helper that operates on WEnv instead of the channel
wenvToLoggerIO :: WEnv -> LoggerIO
wenvToLoggerIO (_, TCommon{tcLogger=ch}) = logCurrentMessage ch

lvlToColor:: LogLevel -> Color
lvlToColor = \case
  LevelDebug -> White
  LevelInfo -> Green
  LevelWarn -> Yellow
  LevelError -> Red
  LevelOther {} -> Blue

startLogger :: Chan WLog -> IO ()
startLogger ch = do
    term <- setupTermFromEnv
    logHandle <- openFile "WhaleChan.log" AppendMode
    hSetBuffering logHandle LineBuffering
    let errOut lvl raw = case getCapability term withForegroundColor of
            Nothing ->
              BS.hPut stderr raw
            Just colored ->
              hRunTermOutput stderr term $
                termText $ colored (lvlToColor lvl) (T.unpack (decodeUtf8 raw))

    void $ forever $ do
        WLog t lvl msg <- readChan ch
        let msg' = utcTimeToLogStr t <> " [" <> logLevelToLogStr lvl <> "] " <> msg <> "\n"
            raw = fromLogStr msg'
        BS.hPut logHandle raw
        errOut lvl raw
