{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , NamedFieldPuns
  #-}
module Javran.WhaleChan.FromSource.Util
  ( dTell
  , expectOne
  , fetchUrl
  , Manager
  ) where

import Control.Monad
import Control.Monad.Writer
import Network.HTTP.Client

import qualified Data.DList as DL
import qualified Data.ByteString.Lazy as BSL

import Javran.WhaleChan.Util

expectOne :: (MonadWriter (DL.DList String) m, Show a) => String -> [a] -> m (Maybe a)
expectOne tag [] = dTell ("no parse for " ++ tag) >> pure Nothing
expectOne tag (x:xs) = do
    unless (null xs) $
      dTell $ "extra result for " ++ tag ++ ": " ++ show xs
    pure (Just x)

fetchUrl :: Manager -> String -> IO BSL.ByteString
fetchUrl mgr url = do
    req <- parseRequest url
    responseBody <$> httpLbs req mgr

