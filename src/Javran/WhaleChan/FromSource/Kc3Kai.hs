{-# LANGUAGE
    OverloadedStrings
  #-}
module Javran.WhaleChan.FromSource.Kc3Kai where

import Data.Aeson

data KC3TimeRaw = KC3TimeRaw String String

instance FromJSON KC3TimeRaw where
  parseJSON = withObject "KC3TimeRaw" $ \o ->
      KC3TimeRaw <$> o .: "maintenance_start"
                 <*> o .: "maintenance_end"
