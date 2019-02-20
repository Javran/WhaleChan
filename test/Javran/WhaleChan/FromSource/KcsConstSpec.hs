module Javran.WhaleChan.FromSource.KcsConstSpec where

import Test.Hspec

import Javran.WhaleChan.FromSource.TimeFormat
import Javran.WhaleChan.FromSource.KcsConst

spec :: Spec
spec =
    describe "parseTime" $
      specify "samples" $ do
        parseTime "2019/02/08 11:00:00" `shouldBe`
          Just (mkUtcInJst 2019 02 08 11 00 00)
        parseTime "2019/02/08 20:25:00" `shouldBe`
          Just (mkUtcInJst 2019 02 08 20 25 00)
        parseTime "invalid?" `shouldBe` Nothing
