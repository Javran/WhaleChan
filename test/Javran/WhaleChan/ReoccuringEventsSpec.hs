module Javran.WhaleChan.ReoccuringEventsSpec where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime

import Test.Hspec

import Javran.WhaleChan.ReoccuringEvents

spec :: Spec
spec = do
    let mk year month day hh mm ss =
          LocalTime (fromGregorian year month day) (TimeOfDay hh mm ss)
    describe "nextPracticeReset" $ do
        specify "trivial" $
          nextPracticeReset (mk 2019 01 02 20 21 22) `shouldBe` mk 2019 01 03 3 0 0
