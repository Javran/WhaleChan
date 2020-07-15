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
    TgRMTweetCreate stId content preview ->
      "MsgTweetCreate"
      <> "{ statusId=" <> ps stId
      <> ", content=" <> shortContent content
      <> ", preview=" <> ps preview
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
    TgRMServerStat content ->
      "MsgServerStat"
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
    sendTgMessage msgContent modifyRequest afterSuccess =
      let req = modifyRequest $ sendMessageRequest chatId msgContent
      in sendMessage tok req tcManager >>= \case
        Right resp -> afterSuccess resp
        Left err -> logErr $ displayException err
    telegramStep = do
        msg <- readChan tcTelegram
        logInfo $ T.unpack $ describeMessage msg
        if tokenIsEmpty
          then logInfo "token is empty, actual call omitted."
          else case msg of
            TgRMTimer content pm ->
                sendTgMessage
                  content
                  (\r -> r {message_parse_mode=pm})
                  (\_ -> pure ())
            TgRMTweetCreate stId content preview ->
                sendTgMessage
                  content
                  (\r -> r { message_parse_mode = Just Markdown
                           , message_disable_web_page_preview = Just (not preview)
                           }) $
                  \Response {result = Message {message_id}} ->
                    putTwMsg tcTwitter (TwRMTgSent message_id stId)
            TgRMTweetDestroy stId msgId ->
                -- TODO: twitter api doesn't seem to guarantee consistency so that
                -- some tweets are mistakenly flagged as removed while in reality it might be
                -- some delay before data is consistent between serving servers.
                -- to fix this, we can probably define a timeout and only send deletion notification
                -- after a tweet is missing and the timeout is passed.
                sendTgMessage
                  "This tweet is deleted."
                  (\r -> r {message_reply_to_message_id = Just msgId}) $
                  \Response {result = Message {message_id}} ->
                    putTwMsg tcTwitter (TwRMTgSent message_id stId)
            TgRMProfileImg {-imgData-} imgUrl ->
                -- for now loading always fails. use url instead
                -- until we can figure out how to do this properly
                sendTgMessage
                  ("\\[Profile] " <> "[Source](" <> imgUrl <> ")")
                  (\r -> r {message_parse_mode = Just Markdown})
                  (\_ -> pure ())
            TgRMProfileStat content ->
                sendTgMessage
                  content
                  (\r -> r {message_parse_mode = Just Markdown})
                  (\_ -> pure ())
            TgRMServerStat content ->
                sendTgMessage
                  content
                  (\r -> r {message_parse_mode = Just Markdown})
                  (\_ -> pure ())

