module Javran.WhaleChan.FromSource.Kc3KaiSpec where

import Data.Aeson.Types
import Test.Hspec

import Javran.WhaleChan.FromSource.TimeFormat
import Javran.WhaleChan.FromSource.Kc3Kai

spec :: Spec
spec = do
    let p = parseMaybe parseTime
    describe "parseTime" $
      specify "samples" $ do
        p "Fri, 08 February 2019 11:00:00 +0900" `shouldBe`
          Just (mkUtcInJst 2019 02 08 11 00 00)
        p "Fri, 08 February 2019 21:00:00 +0900" `shouldBe`
          Just (mkUtcInJst 2019 02 08 21 00 00)
        p "invalid?" `shouldBe` Nothing
