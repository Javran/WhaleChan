{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , NoMonomorphismRestriction
  #-}
module Javran.WhaleChan.FromSource.Kcwiki where

import Control.Arrow
import Control.Monad.Writer
import Text.HTML.DOM
import Text.XML.Cursor

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import Javran.WhaleChan.FromSource.Util
import Javran.WhaleChan.FromSource.TimeFormat

parseTime :: BSL.ByteString -> (Maybe T.Text, [String])
parseTime =
    second (`appEndo` [])
    . runWriter
    . expectOne "start time"
    . ($// searchAndExtract)
    . fromDocument
    . parseLBS

searchAndExtract :: Cursor -> [T.Text]
searchAndExtract =
    element "span"
    >=> "class" `attributeIs` "countdown"
    >=> attribute "data-until"

parse :: T.Text -> Maybe UTCTime
parse t = eitherToMaybe $ mkTimeParser "%Y/%-m/%-d %T %z" (T.unpack t)

getInfo :: Manager -> IO (PRange UTCTime)
getInfo mgr = do
    raw <- fetchUrl mgr "https://zh.kcwiki.org/wiki/Template:维护倒数?action=render"
    let (lRaw, _) = parseTime raw
    {-
      TODO as one rhs is always Nothing, we are effectively ignoring this source,
      this can only be restored once Kcwiki support both maintenance start and end time.
     -}
    pure (toPRange (lRaw >>= parse) Nothing)
