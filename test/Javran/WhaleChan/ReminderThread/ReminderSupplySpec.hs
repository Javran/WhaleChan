module Javran.WhaleChan.ReminderThread.ReminderSupplySpec where

import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec

import Javran.WhaleChan.ReminderThread.Types
import Javran.WhaleChan.ReminderThread.EventReminder

spec :: Spec
spec = do
  let {-
         create time in UTC.
         for the test we only care that:
         - due list are arranged in a sorted manner
         - no duplication in due list
         - eventTime is indeed present even when not given as input
         all of which can be captured by giving one example and
         expect output to be an exact match.
       -}
      mk year month day hh mm =
        localTimeToUTC utc $
          LocalTime (fromGregorian year month day) (TimeOfDay hh mm 0)
  describe "createEventReminderWithDueList" $
    specify "sample" $ do
      let eventTime = mk 2019 3 21 10 20
          er = createEventReminderWithDueList eventTime [60*8,60*40,10,60,60*20]
      er `shouldBe`
        makeEventReminder eventTime
          [ mk 2019 3 19 18 20 -- 40 hours prior
          , mk 2019 3 20 14 20 -- 20 hours prior
          , mk 2019 3 21 2 20 -- 8 hours prior
          , mk 2019 3 21 9 20 -- 1 hour prior
          , mk 2019 3 21 10 10 -- 10 mins prior
          , eventTime -- happening
          ]
