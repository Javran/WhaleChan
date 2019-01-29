{-# LANGUAGE TypeApplications, DataKinds #-}
module Javran.WhaleChan.ReminderSupply
  where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import Javran.WhaleChan.Types
import Javran.WhaleChan.ReoccuringEvents

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

renewSupplyByFunc :: (LocalTime -> LocalTime) -> TimeZoneSeries -> UTCTime -> EventReminder
renewSupplyByFunc getNextTime tzs ut =
    EventReminder
      eventTime
      -- TOOD: the 24 hours ahead is for debugging purpose and we might remove that when done
      [mkTime (24 * 60), mkTime 30, mkTime 10, mkTime 5, eventTime]
  where
    toLocal = utcToLocalTime' tzs
    fromLocal = localTimeToUTC' tzs
    eventTime :: UTCTime
    eventTime = fromLocal . getNextTime . toLocal $ ut
    mkTime mins = addUTCTime (fromIntegral @Int $ -60 * mins) eventTime

instance ReminderSupply 'PracticeReset where
    renewSupply _ = renewSupplyByFunc nextPracticeReset

instance ReminderSupply 'DailyQuestReset where
    renewSupply _ = renewSupplyByFunc nextDailyQuestReset

instance ReminderSupply 'WeeklyQuestReset where
    renewSupply _ = renewSupplyByFunc nextWeeklyQuestReset

instance ReminderSupply 'MonthlyQuestReset where
    renewSupply _ = renewSupplyByFunc nextMonthlyQuestReset

instance ReminderSupply 'QuarterlyQuestReset where
    renewSupply _ = renewSupplyByFunc nextQuarterlyQuestReset

instance ReminderSupply 'ExtraOperationReset where
    renewSupply _ = renewSupplyByFunc nextExtraOperationReset

instance ReminderSupply 'SenkaAccounting where
    renewSupply _ = renewSupplyByFunc nextSenkaAccounting

instance ReminderSupply 'QuestPointDeadline where
    renewSupply _ = renewSupplyByFunc nextQuestPointDeadline
