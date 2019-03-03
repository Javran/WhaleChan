module Javran.WhaleChan.UtilSpec where

import Test.Hspec

import Javran.WhaleChan.Util

spec :: Spec
spec = describe "describeDuration" $
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
