{-# LANGUAGE TypeApplications, DataKinds #-}
module Javran.WhaleChan.ReminderSupply
  ( ReminderSupplier(..)
  , createEventReminderWithDueList
  ) where

import Data.List
import qualified Data.List.Ordered as LO
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import Javran.WhaleChan.Types
import Javran.WhaleChan.ReoccuringEvents

-- TODO: should we switch to use symbols rather than promoted data type?
data ReminderSupplier
  = PracticeReset
  | DailyQuestReset
  | WeeklyQuestReset
  | MonthlyQuestReset
  | QuarterlyQuestReset
  | ExtraOperationReset
  | SenkaAccounting
  | QuestPointDeadline
    deriving (Enum, Eq, Ord, Bounded)

-- | create a EventReminder with supplied eventTime and a list
--   of due times (in minutes) by which the reminder should be sent out
--   prior to the event time.
--   note that in the resulting list, eventTime is always in the list of
--   dueTimes so you don't have to pass that as argument explicitly.
createEventReminderWithDueList :: UTCTime -> [Int] -> EventReminder
createEventReminderWithDueList eventTime dueListPre =
    EventReminder
      eventTime
      (mkTime <$> dueList)
  where
    -- descending list of time without duplicated elements
    dueList = LO.nub $ sortBy (flip compare) (0 : dueListPre)
    mkTime mins = addUTCTime (fromIntegral @Int $ -60 * mins) eventTime

renewSupplyByFunc :: (LocalTime -> LocalTime) -> TimeZoneSeries -> UTCTime -> EventReminder
renewSupplyByFunc getNextTime tzs ut =
    createEventReminderWithDueList eventTime [24*60, 30, 10, 5]
  where
    eventTime :: UTCTime
    eventTime =
      localTimeToUTC' tzs
      . getNextTime
      . utcToLocalTime' tzs
      $ ut

instance ReminderSupply 'PracticeReset where
    renewSupply _ = renewSupplyByFunc nextPracticeReset
    eventDescription _ = "Practice Reset"

instance ReminderSupply 'DailyQuestReset where
    renewSupply _ = renewSupplyByFunc nextDailyQuestReset
    eventDescription _ = "Daily Quest Reset"

instance ReminderSupply 'WeeklyQuestReset where
    renewSupply _ = renewSupplyByFunc nextWeeklyQuestReset
    eventDescription _ = "Weekly Quest Reset"

instance ReminderSupply 'MonthlyQuestReset where
    renewSupply _ = renewSupplyByFunc nextMonthlyQuestReset
    eventDescription _ = "Monthly Quest Reset"

instance ReminderSupply 'QuarterlyQuestReset where
    renewSupply _ = renewSupplyByFunc nextQuarterlyQuestReset
    eventDescription _ = "Quarterly Quest Reset"

instance ReminderSupply 'ExtraOperationReset where
    renewSupply _ = renewSupplyByFunc nextExtraOperationReset
    eventDescription _ = "Extra Operation Reset"

instance ReminderSupply 'SenkaAccounting where
    renewSupply _ = renewSupplyByFunc nextSenkaAccounting
    eventDescription _ = "Senka Accounting"

instance ReminderSupply 'QuestPointDeadline where
    renewSupply _ = renewSupplyByFunc nextQuestPointDeadline
    eventDescription _ = "Quest Point Deadline"
