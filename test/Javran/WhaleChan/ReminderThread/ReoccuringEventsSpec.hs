module Javran.WhaleChan.ReminderThread.ReoccuringEventsSpec where

import Control.Monad
import Data.Time.Calendar
import Data.Time.LocalTime

import Test.Hspec

import Javran.WhaleChan.ReminderThread.ReoccuringEvents

spec :: Spec
spec = do
    let mk year month day hh mm ss =
          LocalTime (fromGregorian year month day) (TimeOfDay hh mm ss)
        mkTest f inp expected = f inp `shouldBe` expected
    describe "nextPracticeReset" $ do
      let t = mkTest nextPracticeReset
      specify "trivials" $ do
        t (mk 2019 01 02 20 21 22)
          (mk 2019 01 03 03 00 00)
        t (mk 2019 01 02 14 15 16)
          (mk 2019 01 02 15 00 00)
      specify "exact times" $ do
        t (mk 2019 01 02 03 00 00)
          (mk 2019 01 02 15 00 00)
        t (mk 2019 01 02 15 00 00)
          (mk 2019 01 03 03 00 00)

    describe "nextDailyQuestReset" $ do
      let t = mkTest nextDailyQuestReset
      specify "trivials" $ do
        t (mk 2019 01 02 04 59 59)
          (mk 2019 01 02 05 00 00)
        t (mk 2019 01 02 21 00 23)
          (mk 2019 01 03 05 00 00)
      specify "exact times" $
        t (mk 2019 01 02 05 00 00)
          (mk 2019 01 03 05 00 00)

    describe "nextWeeklyQuestReset" $ do
      {-
            January 2019
        Su Mo Tu We Th Fr Sa
               1  2  3  4  5
         6  7  8  9 10 11 12
        13 14 15 16 17 18 19

       -}
      let t = mkTest nextWeeklyQuestReset
      specify "trivials" $ do
        t (mk 2019 01 07 04 59 59)
          (mk 2019 01 07 05 00 00)
        t (mk 2019 01 04 12 34 56)
          (mk 2019 01 07 05 00 00)
        t (mk 2019 01 04 12 34 56)
          (mk 2019 01 07 05 00 00)
        t (mk 2019 01 10 12 00 00)
          (mk 2019 01 14 05 00 00)
      specify "exact times" $ do
        t (mk 2018 12 31 05 00 00)
          (mk 2019 01 07 05 00 00)
        t (mk 2019 01 07 05 00 00)
          (mk 2019 01 14 05 00 00)

    describe "nextMonthlyQuestReset" $ do
      let t = mkTest nextMonthlyQuestReset
      specify "trivials" $ do
        t (mk 2019 01 01 04 59 59)
          (mk 2019 01 01 05 00 00)
        t (mk 2019 01 29 12 34 56)
          (mk 2019 02 01 05 00 00)
      specify "leap year" $
        t (mk 2020 02 28 12 34 56)
          (mk 2020 03 01 05 00 00)
      specify "exact times" $ do
        t (mk 2019 01 01 05 00 00)
          (mk 2019 02 01 05 00 00)
        t (mk 2018 12 01 05 00 00)
          (mk 2019 01 01 05 00 00)

    describe "nextQuarterlyQuestReset" $ do
      let t = mkTest nextQuarterlyQuestReset
      specify "trivials" $ do
        forM_ [01,02,03] $ \mm ->
          t (mk 2019 mm 01 04 59 59)
            (mk 2019 03 01 05 00 00)
        forM_ [04,05,06] $ \mm ->
          t (mk 2019 mm 01 04 59 59)
            (mk 2019 06 01 05 00 00)
        forM_ [07,08,09] $ \mm ->
          t (mk 2019 mm 01 04 59 59)
            (mk 2019 09 01 05 00 00)
        forM_ [10,11,12] $ \mm ->
          t (mk 2019 mm 01 04 59 59)
            (mk 2019 12 01 05 00 00)
      specify "2nd day of months" $ do
        t (mk 2019 03 02 00 00 00)
          (mk 2019 06 01 05 00 00)
        t (mk 2019 06 02 00 00 00)
          (mk 2019 09 01 05 00 00)
        t (mk 2019 09 02 00 00 00)
          (mk 2019 12 01 05 00 00)
        t (mk 2019 12 02 00 00 00)
          (mk 2020 03 01 05 00 00)

      specify "exact times" $ do
        t (mk 2019 03 01 05 00 00)
          (mk 2019 06 01 05 00 00)
        t (mk 2019 06 01 05 00 00)
          (mk 2019 09 01 05 00 00)
        t (mk 2019 12 01 05 00 00)
          (mk 2020 03 01 05 00 00)

    describe "nextExtraOperationReset" $ do
      let t = mkTest nextExtraOperationReset
      specify "trivials" $ do
        t (mk 2019 01 31 23 59 59)
          (mk 2019 02 01 00 00 00)
        t (mk 2019 02 28 23 59 59)
          (mk 2019 03 01 00 00 00)
      specify "exact times" $ do
        t (mk 2019 02 01 00 00 00)
          (mk 2019 03 01 00 00 00)
        t (mk 2018 12 01 00 00 00)
          (mk 2019 01 01 00 00 00)

    describe "nextSenkaAccounting" $ do
      let t = mkTest nextSenkaAccounting
      specify "trivials" $ do
        t (mk 2019 02 28 00 00 00)
          (mk 2019 02 28 02 00 00)
        t (mk 2019 02 28 02 01 00)
          (mk 2019 02 28 14 00 00)
        t (mk 2019 02 28 14 01 00)
          (mk 2019 02 28 22 00 00)
        t (mk 2019 02 28 22 00 01)
          (mk 2019 03 01 02 00 00)

        t (mk 2019 01 20 00 00 01)
          (mk 2019 01 20 02 00 00)
        t (mk 2019 01 20 02 00 01)
          (mk 2019 01 20 14 00 00)
        t (mk 2019 01 20 14 00 01)
          (mk 2019 01 21 02 00 00)
      specify "exact times" $ do
        t (mk 2019 02 27 14 00 00)
          (mk 2019 02 28 02 00 00)
        t (mk 2019 02 28 02 00 00)
          (mk 2019 02 28 14 00 00)
        t (mk 2020 02 28 02 00 00)
          (mk 2020 02 28 14 00 00)
        t (mk 2020 02 28 14 00 00)
          (mk 2020 02 29 02 00 00)
        t (mk 2020 02 29 02 00 00)
          (mk 2020 02 29 14 00 00)
        t (mk 2020 02 29 14 00 00)
          (mk 2020 02 29 22 00 00)
        t (mk 2020 02 29 22 00 00)
          (mk 2020 03 01 02 00 00)

    describe "nextQuestPointDeadlineReset" $ do
      let t = mkTest nextQuestPointDeadline
      specify "trivials" $ do
        t (mk 2019 01 23 12 34 56)
          (mk 2019 01 31 14 00 00)
        t (mk 2019 01 31 14 00 01)
          (mk 2019 02 28 14 00 00)
      specify "exact times" $ do
        t (mk 2019 02 28 14 00 00)
          (mk 2019 03 31 14 00 00)
        t (mk 2020 01 31 14 00 00)
          (mk 2020 02 29 14 00 00)
