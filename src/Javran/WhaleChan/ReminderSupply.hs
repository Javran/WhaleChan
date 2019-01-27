{-# LANGUAGE TypeApplications #-}
module Javran.WhaleChan.ReminderSupply
  ( DailyQuestReset
  ) where

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

data DailyQuestReset

instance ReminderSupply DailyQuestReset where
    renewSupply _ = renewSupplyByFunc nextDailyQuestReset
