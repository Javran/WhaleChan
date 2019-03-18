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
import Javran.WhaleChan.FromSource.TimeFormat

parseTime :: BSL.ByteString -> ((Maybe T.Text, Maybe T.Text), [String])
parseTime =
    second (`appEndo` [])
    . collectResult
    . ($// searchAndExtractCountdownStrs)
    . fromDocument
    . parseLBS

collectResult :: [(Bool, T.Text)] -> ((Maybe T.Text, Maybe T.Text), Endo [String])
collectResult xs
  | (ends{- True (in front) for end-}, starts) <- partition fst xs
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

parse :: T.Text -> Maybe UTCTime
parse t = eitherToMaybe $ mkTimeParser "%B %-d %Y %T %z" (T.unpack t)

getInfo :: Manager -> IO (PRange UTCTime)
getInfo mgr = do
    raw <- fetchUrl mgr "https://kancolle.fandom.com/wiki/Recent_Updates?action=render"
    -- TODO: error message
    let ((lRaw, rRaw), _) = parseTime raw
    pure (toPRange (lRaw >>= parse) (rRaw >>= parse))
