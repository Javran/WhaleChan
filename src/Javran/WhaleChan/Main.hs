{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , OverloadedStrings
  #-}
module Javran.WhaleChan.Main
  ( timerThread
  , main
  ) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.State
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment
import System.Exit
import qualified Web.Telegram.API.Bot as Tg

import Javran.WhaleChan.Base
import Javran.WhaleChan.TimerThread (timerThread)
import Javran.WhaleChan.Types

{-
  architecture draft:

  twitter api thread

  - for each 10 seconds, retrieve without since_id and count=200

    + the rate limit is 900 for a 15min window, we'll be using 6 per min * 15 = 90 calls,
      way below the limit for a decent response time.
    + the intention of not using since_id is to retrieve tweet ids
      of the past so that deletion can be detected
    + talk to telegram api thread when there's a need of sending messages

  telegram api thread

  - assume no user interaction, the mere task is to post to the channel
  - wait for messages to arrive and then send

  scheduler thread

  - for reminder of reoccurring events
  - 30 mins before
  - 5 mins before
  - right now

  main thread into a sleep loop, check thread health occasionally

 -}

telegramThread :: Manager -> Chan T.Text -> Tg.Token -> Int64 -> IO ()
telegramThread mgr msgChan tok channelId = forever $ do
    msg <- readChan msgChan
    let req = Tg.SendMessageRequest
                { Tg.message_chat_id = Tg.ChatId channelId
                , Tg.message_text = msg
                , Tg.message_parse_mode = Nothing
                , Tg.message_disable_web_page_preview = Nothing
                , Tg.message_disable_notification = Nothing
                , Tg.message_reply_to_message_id = Nothing
                , Tg.message_reply_markup = Nothing
                }
    result <- Tg.sendMessage tok req mgr
    print result

startService :: WEnv -> IO ()
startService wenv = do
  mgr <- newManager tlsManagerSettings
  ch <- newChan
  let WEnv {tgBotToken=botToken, tgChannelId=tgChannelId} = wenv
  aTimer <- async (evalStateT (timerThread ch) M.empty)
  aTg <- async (telegramThread mgr ch botToken tgChannelId)
  wait aTimer
  wait aTg

{-
  events to be implemented:

  - [x] daily quest reset
  - [x] practice reset
  - [x] senka accounting (3 times at the end of each month)
  - [x] EO reset
  - [x] quest senka freeze
  - [ ] secretary & comment accounting
  - [x] weekly quest reset
  - [x] monthly quest reset
  - [x] quaterly quest reset: 3 6 9 12

 -}

-- ref: https://stackoverflow.com/q/43835656/315302

main :: IO ()
main = getArgs >>= \case
    [cfg] -> loadWEnv cfg >>= startService
    _ -> putStrLn "WhaleChan <config.yaml>" >> exitFailure
