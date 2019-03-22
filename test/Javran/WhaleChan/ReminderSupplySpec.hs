module Javran.WhaleChan.ReminderSupplySpec where

import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec

import Javran.WhaleChan.Types
import Javran.WhaleChan.ReminderSupply

spec :: Spec
spec = do
  let mk year month day hh mm =
        localTimeToUTC utc $
          LocalTime (fromGregorian year month day) (TimeOfDay hh mm 0)
  describe "createEventReminderWithDueList" $
    specify "sample" $ do
      let eventTime = mk 2019 3 21 10 20
          er = createEventReminderWithDueList eventTime [60*8,60*40,10,60,60*20]
      er `shouldBe`
        EventReminder eventTime
          [ mk 2019 3 19 18 20 -- 40 hours prior
          , mk 2019 3 20 14 20 -- 20 hours prior
          , mk 2019 3 21 2 20 -- 8 hours prior
          , mk 2019 3 21 9 20 -- 1 hour prior
          , mk 2019 3 21 10 10 -- 10 mins prior
          , eventTime -- happening
          ]
