{-# LANGUAGE OverloadedStrings #-}
module Javran.WhaleChan.UtilSpec where

import Test.Hspec
import Data.Aeson
import qualified Data.HashMap.Strict as HM

import Javran.WhaleChan.Util

spec :: Spec
spec = do
  describe "describeDuration" $
    specify "cases" $ do
      let t h m s expected = describeDuration (h*3600+m*60+s) `shouldBe` expected
      t 12 34 56 "12 hours 34 minutes 56 seconds"
      t 25 34 56 "1 day 1 hour 34 minutes 56 seconds"
      t 24 00 00 "1 day"
      t 48 00 00 "2 days"
      t 00 34 56 "34 minutes 56 seconds"
      t 01 00 56 "1 hour 56 seconds"
      t 00 00 01 "1 second"
      t 00 00 00 "0 seconds"
      t 02 00 00 "2 hours"
      t 00 00 59 "59 seconds"
      t 01 01 01 "1 hour 1 minute 1 second"
  describe "flattenJson" $
    specify "cases" $ do
      let t raw expected = flattenJson m `shouldBe` HM.fromList expected
            where
              Just (Object m) = decodeStrict raw :: Maybe Value
      t "{}" []
      t "{\"a\":{\"b\":1,\"c\":{\"d\":2,\"e\":3}}}"
        [("a.c.e","3.0"),("a.b","1.0"),("a.c.d","2.0")]
      t "{\"a\":[1,2,\"e\",null,{}]}"
        [("a","[1.0,2.0,e,Null,[]]")]

