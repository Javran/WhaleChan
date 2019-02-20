{-# LANGUAGE OverloadedStrings #-}
module Javran.WhaleChan.FromSource.KcwikiSpec where

import Test.Hspec

import Javran.WhaleChan.FromSource.TimeFormat
import Javran.WhaleChan.FromSource.Kcwiki

spec :: Spec
spec =
    describe "parse" $
      specify "samples" $ do
        parse "2019/2/27 11:00:00 +0900" `shouldBe`
          Just (mkUtcInJst 2019 02 27 11 00 00)
        parse "invalid?" `shouldBe` Nothing
