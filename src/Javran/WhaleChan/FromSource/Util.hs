{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}
module Javran.WhaleChan.FromSource.Util
  ( tell'
  , expectOne
  ) where

import Control.Monad
import Control.Monad.Writer

tell' :: MonadWriter (Endo [a]) m => a -> m ()
tell' x = tell $ Endo ([x] ++)

expectOne :: (MonadWriter (Endo [String]) m, Show a) => String -> [a] -> m (Maybe a)
expectOne tag [] = tell' ("no parse for " ++ tag) >> pure Nothing
expectOne tag (x:xs) = do
    unless (null xs) $
      tell' $ "extra result for " ++ tag ++ ": " ++ show xs
    pure (Just x)
