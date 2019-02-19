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

import Javran.WhaleChan.Util

parseKcwikiMaintenanceStartTime :: BSL.ByteString -> (Maybe T.Text, [String])
parseKcwikiMaintenanceStartTime =
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
