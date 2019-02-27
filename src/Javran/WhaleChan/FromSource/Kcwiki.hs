{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , NoMonomorphismRestriction
  , TupleSections
  #-}
module Javran.WhaleChan.FromSource.Kcwiki where

import Control.Applicative
import Control.Arrow
import Control.Monad.Writer
import Text.HTML.DOM
import Text.XML.Cursor

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import Javran.WhaleChan.FromSource.Util
import Javran.WhaleChan.FromSource.TimeFormat
import Data.List

-- TODO: share code with Wikia

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
    >=> (\(b,t) -> (b,) <$> (t $| attribute "data-until"))
  where
    -- extract only relevant tags with tag attached to distinguish between start and end
    checkAndTag =
        (<|>) <$> (("data-type" `attributeIs` "maint-start") &| (False,))
              <*> (("data-type" `attributeIs` "maint-end") &| (True,))

searchAndExtract :: Cursor -> [T.Text]
searchAndExtract =
    element "span"
    >=> "class" `attributeIs` "countdown"
    >=> attribute "data-until"

parse :: T.Text -> Maybe UTCTime
parse t = eitherToMaybe $ mkTimeParser "%Y/%-m/%-d %T %z" (T.unpack t)

getInfo :: Manager -> IO (PRange UTCTime)
getInfo mgr = do
    raw <- fetchUrl mgr "https://zh.kcwiki.org/wiki/Template:维护倒数?action=raw"
    let ((lRaw, rRaw), _) = parseTime raw
    pure (toPRange (lRaw >>= parse) (rRaw >>= parse))
