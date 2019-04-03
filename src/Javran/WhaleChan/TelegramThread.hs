{-# LANGUAGE
    NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , LambdaCase
  , OverloadedStrings
  , NoMonomorphismRestriction
  #-}
module Javran.WhaleChan.TelegramThread
  ( telegramThread
  ) where

import Control.Concurrent.Chan
import Control.Exception.Base
import Control.Monad
import Control.Monad.RWS
import Web.Telegram.API.Bot
-- import Network.Mime
-- import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Javran.WhaleChan.TweetSyncThread
import Javran.WhaleChan.Types
import Javran.WhaleChan.Base
import qualified Javran.WhaleChan.Log as Log

{-
  telegram thread listens on its own channel
  and send message to channel as needed.
  the thread structure is simple thus does not require to use WCM at all.
 -}

describeMessage :: TgRxMsg -> T.Text
describeMessage = \case
    TgRMTimer content pMode ->
      "MsgTimer"
      <> "{ parseMode=" <> ps pMode
      <> ", content=" <> shortContent content
      <> "}"
    TgRMTweetCreate stId content ->
      "MsgTweetCreate"
      <> "{ statusId=" <> ps stId
      <> ", content=" <> shortContent content
      <> "}"
    TgRMTweetDestroy stId tgId ->
      "MsgTweetDestroy"
      <> "{ statusId=" <> ps stId
      <> ", tgId=" <> ps tgId
      <> "}"
    TgRMProfileImg content ->
      "MsgProfileImg"
      <> "{ content=" <> shortContent content
      <> "}"
    TgRMProfileStat content ->
      "MsgProfileStat"
      <> "{ content=" <> shortContent content
      <> "}"
  where
    ps = T.pack . show
    shortContent s =
        if l < 1000
          then s
          else "(content too long, total length=" <> T.pack (show l) <> ")"
      where
        l = T.length s

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
        logInfo $ T.unpack $ describeMessage msg
        if tokenIsEmpty
          then logInfo "token is empty, actual call omitted."
          else case msg of
            TgRMTimer t pm -> do
                let req = (sendMessageRequest chatId t) {message_parse_mode=pm}
                r <- sendMessage tok req tcManager
                case r of
                  Right _ -> pure ()
                  Left e -> do
                    logErr (displayException e)
                    logErr $ "request is: " <> show req
            TgRMTweetCreate stId content -> do
                let req = (sendMessageRequest chatId content)
                           { message_parse_mode = Just Markdown
                           , message_disable_web_page_preview = Just True
                           }
                sendMessage tok req tcManager >>= \case
                   Right Response {result = Message {message_id}} ->
                    putTwMsg tcTwitter (TwRMTgSent message_id stId)
                   Left err -> logErr $ displayException err
            TgRMTweetDestroy stId msgId ->
                sendMessageSimple "This tweet is deleted." (Just msgId) >>= \case
                  Right Response {result = Message {message_id}} ->
                    putTwMsg tcTwitter (TwRMTgSent message_id stId)
                  Left err -> logErr $ displayException err
            TgRMProfileImg {-imgData-} imgUrl -> do
                 let content = "\\[Profile] " <> "[Source](" <> imgUrl <> ")"
                     req = (sendMessageRequest chatId content)
                           {message_parse_mode = Just Markdown}
                 sendMessage tok req tcManager >>= \case
                   Right _ -> pure ()
                   Left err -> logErr $ displayException err
                 -- for now loading always fails. use url instead
                 -- until we can figure out how to do this properly
            TgRMProfileStat content -> do
                 let req = (sendMessageRequest chatId content)
                           {message_parse_mode = Just Markdown}
                 sendMessage tok req tcManager >>= \case
                   Right _ -> pure ()
                   Left err -> logErr $ displayException err
