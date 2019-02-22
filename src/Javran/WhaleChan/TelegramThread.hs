{-# LANGUAGE
    NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , LambdaCase
  , OverloadedStrings
  #-}
module Javran.WhaleChan.TelegramThread
  ( telegramThread
  ) where

import Control.Concurrent.Chan
import Control.Exception.Base
import Control.Monad
import Control.Monad.RWS
import Web.Telegram.API.Bot

import qualified Data.Text as T

import Javran.WhaleChan.TweetSyncThread
import Javran.WhaleChan.Types
import Javran.WhaleChan.Base
import Javran.WhaleChan.Util
import qualified Javran.WhaleChan.Log as Log

{-
  telegram thread listens on its own channel
  and send message to channel as needed.
  the thread structure is simple thus does not require to use WCM at all.
 -}

telegramThread :: WEnv -> IO ()
telegramThread wenv@(wconf, tcomm) =
    protectedAction loggerIO "TelegramThread" 16 $
      void (forever telegramStep)
  where
    loggerIO = wenvToLoggerIO wenv
    logInfo = Log.i' loggerIO "TelegramThread"
    logErr = Log.e' loggerIO "TelegramThread"
    WConf { tgBotToken = tok@(Token tokContent)
          , tgChannelId
          } = wconf
    TCommon {tcTelegram, tcTwitter, tcManager} = tcomm
    tokenIsEmpty = T.null tokContent
    chatId = ChatId tgChannelId
    sendMessageSimple msg replyTo = liftIO $ sendMessage tok req tcManager
      where
        req = SendMessageRequest
              { message_chat_id = chatId
              , message_text = msg
              , message_parse_mode = Nothing
              , message_disable_web_page_preview = Nothing
              , message_disable_notification = Nothing
              , message_reply_to_message_id = replyTo
              , message_reply_markup = Nothing
              }
    telegramStep = do
        msg <- readChan tcTelegram
        if tokenIsEmpty
          then logInfo $ "EmptyToken. Received request: " <> show msg
          else case msg of
            TgRMTimer t pm ->
                let req = (sendMessageRequest chatId t) {message_parse_mode=pm}
                in void $ sendMessage tok req tcManager
            TgRMTweetCreate stId t ->
                sendMessageSimple t Nothing >>= \case
                  Right Response {result = Message {message_id}} ->
                    putTwMsg tcTwitter (TwRMTgSent message_id stId)
                  Left err -> logErr $ displayException err
            TgRMTweetDestroy stId msgId ->
                sendMessageSimple "This tweet is deleted." (Just msgId) >>= \case
                  Right Response {result = Message {message_id}} ->
                    putTwMsg tcTwitter (TwRMTgSent message_id stId)
                  Left err -> logErr $ displayException err
