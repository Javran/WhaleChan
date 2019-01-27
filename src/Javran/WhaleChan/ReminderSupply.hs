{-# LANGUAGE TypeApplications #-}
module Javran.WhaleChan.ReminderSupply
  where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import Javran.WhaleChan.Types
import Javran.WhaleChan.ReoccuringEvents

renewSupplyByFunc :: (LocalTime -> LocalTime) -> TimeZoneSeries -> UTCTime -> [UTCTime]
renewSupplyByFunc getNextTime tzs ut =
    [mkTime 30, mkTime 10, mkTime 5, mkTime 0]
  where
    toLocal = utcToLocalTime' tzs
    fromLocal = localTimeToUTC' tzs
    eventTime :: UTCTime
    eventTime = fromLocal . getNextTime . toLocal $ ut
    mkTime mins = addUTCTime (fromIntegral @Int $ -60 * mins) eventTime

data PracticeReset
instance ReminderSupply PracticeReset where
    renewSupply _ = renewSupplyByFunc nextPracticeReset

data DailyQuestReset
instance ReminderSupply DailyQuestReset where
    renewSupply _ = renewSupplyByFunc nextDailyQuestReset

data WeeklyQuestReset
instance ReminderSupply WeeklyQuestReset where
    renewSupply _ = renewSupplyByFunc nextWeeklyQuestReset

data MonthlyQuestReset
instance ReminderSupply MonthlyQuestReset where
    renewSupply _ = renewSupplyByFunc nextMonthlyQuestReset

data QuarterlyQuestReset
instance ReminderSupply QuarterlyQuestReset where
    renewSupply _ = renewSupplyByFunc nextQuarterlyQuestReset

data ExtraOperationReset
instance ReminderSupply ExtraOperationReset where
    renewSupply _ = renewSupplyByFunc nextExtraOperationReset

data SenkaAccounting
instance ReminderSupply SenkaAccounting where
    renewSupply _ = renewSupplyByFunc nextSenkaAccounting

data QuestPointDeadline
instance ReminderSupply QuestPointDeadline where
    renewSupply _ = renewSupplyByFunc nextQuestPointDeadline
