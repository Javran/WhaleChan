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
import Data.Int (Int64)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Web.Telegram.API.Bot
import Javran.WhaleChan.Types
import Say

telegramThread :: Manager -> Chan TgRxMsg -> Chan TwRxMsg -> Token -> Int64 -> IO ()
telegramThread mgr msgChan twMsgChan tok@(Token tokContent) channelId = forever $ do
    msg <- readChan msgChan
    if T.null tokContent
      then sayString $ "[tg] EmptyToken. Received request: " <> show msg
      else case msg of
        TgRMTimer t -> sendMessageSimple t Nothing >>= print
        TgRMTweetCreate t ->
            sendMessageSimple t Nothing >>= \case
              Right Response {result = Message {message_id}} ->
                writeChan twMsgChan (TwRMTgSent message_id)
              Left err -> sayString $ "[tg] error: " <> displayException err
        TgRMTweetDestroy msgId ->
            sendMessageSimple "This tweet is deleted." (Just msgId) >>= \case
              Right Response {result = Message {message_id}} ->
                writeChan twMsgChan (TwRMTgSent message_id)
              Left err -> sayString $ "[tg] error: " <> displayException err
  where
    sendMessageSimple msg replyTo = sendMessage tok req mgr
      where
        req = SendMessageRequest
                { message_chat_id = ChatId channelId
                , message_text = msg
                , message_parse_mode = Nothing
                , message_disable_web_page_preview = Nothing
                , message_disable_notification = Nothing
                , message_reply_to_message_id = replyTo
                , message_reply_markup = Nothing
                }
