{-# LANGUAGE
    NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  #-}
module Javran.WhaleChan.TelegramThread
  ( telegramThread
  ) where

import Control.Concurrent.Chan
import Control.Monad
import Data.Int (Int64)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Web.Telegram.API.Bot

telegramThread :: Manager -> Chan T.Text -> Token -> Int64 -> IO ()
telegramThread mgr msgChan tok channelId = forever $ do
    msg <- readChan msgChan
    let Token tokContent = tok
        req = SendMessageRequest
                { message_chat_id = ChatId channelId
                , message_text = msg
                , message_parse_mode = Nothing
                , message_disable_web_page_preview = Nothing
                , message_disable_notification = Nothing
                , message_reply_to_message_id = Nothing
                , message_reply_markup = Nothing
                }
    if T.null tokContent
      then putStrLn $ "[EmptyToken] send " <> show req
      else sendMessage tok req mgr >>= print
