module Javran.WhaleChan.ServerStatThread.BaseSpec
  ( spec
  ) where

import Test.Hspec

import Javran.WhaleChan.ServerStatThread.Base

spec :: Spec
spec =
  describe "parseServerAddr" $
    specify "sample" $ do
      parseServerAddr "invalid"
        `shouldBe` Nothing
      parseServerAddr "http://255.254.1.0/"
        `shouldBe` Just "255.254.1.0"
      parseServerAddr "http://255.254.1.0.0/"
        `shouldBe` Nothing
      parseServerAddr "http://255.254.1/"
        `shouldBe` Nothing
      parseServerAddr "http://255.254.1.0"
        `shouldBe` Nothing
      parseServerAddr "http://255.-1.1.0"
        `shouldBe` Nothing
      parseServerAddr "http://255.256.1.0"
        `shouldBe` Nothing
