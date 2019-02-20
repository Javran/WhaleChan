{-# LANGUAGE OverloadedStrings #-}
module Javran.WhaleChan.FromSource.WikiaSpec where

import Test.Hspec

import Javran.WhaleChan.FromSource.TimeFormat
import Javran.WhaleChan.FromSource.Wikia

spec :: Spec
spec =
    describe "parse" $
      specify "samples" $ do
        parse "February 27 2019 11:00:00 +0900" `shouldBe`
          Just (mkUtcInJst 2019 02 27 11 00 00)
        parse "February 27 2019 19:00:00 +0900" `shouldBe`
          Just (mkUtcInJst 2019 02 27 19 00 00)
        parse "invalid?" `shouldBe` Nothing
