{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , FlexibleContexts
  #-}
module Javran.WhaleChan.FromSource.Wikia where

import Control.Applicative
import Control.Arrow
import Control.Monad.Writer
import Data.List
import Text.HTML.DOM
import Text.XML.Cursor

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import Javran.WhaleChan.FromSource.Util

parseMaintenanceTime :: BSL.ByteString -> ((Maybe T.Text, Maybe T.Text), [String])
parseMaintenanceTime =
    second (`appEndo` [])
    . collectResult
    . ($// searchAndExtractCountdownStrs)
    . fromDocument
    . parseLBS

collectResult :: [(Bool, T.Text)] -> ((Maybe T.Text, Maybe T.Text), Endo [String])
collectResult xs
  | (starts, ends) <- partition fst xs
  = runWriter $
      (,) <$> expectOne "start time" (snd <$> starts)
          <*> expectOne "end time" (snd <$> ends)

searchAndExtractCountdownStrs :: Cursor -> [(Bool, T.Text)]
searchAndExtractCountdownStrs =
    element "span"
    >=> "class" `attributeIs` "countdown"
    >=> checkAndTag
    >=> (\(b,t) -> (b,) <$> (t $// extractCountdowndate))
  where
    -- extract only relevant tags with tag attached to distinguish between start and end
    checkAndTag =
        (<|>) <$> (("data-toggle" `attributeIs` ".maint-start") &| (False,))
              <*> (("data-toggle" `attributeIs` ".maint-end") &| (True,))
    extractCountdowndate =
        element "span"
        >=> "class" `attributeIs` "countdowndate"
        >=> child >=> content
