{-# LANGUAGE
    TypeApplications
  , DataKinds
  , ExistentialQuantification
  , PolyKinds
  , DefaultSignatures
  #-}

module Javran.WhaleChan.ReminderThread.Types
  ( ReminderSupplier(..)
  , createEventReminderWithDueList
  , ReminderSupply
  , renewSupply
  , eventDescription
  , EReminderSupply(..)
  , reminderSupplies
  , reminders
  , strToReminderTypeRep
  -- , checkEventReminder
  ) where

import Control.Monad
import Data.Aeson.Types (Parser)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series
import Data.Typeable

import Javran.WhaleChan.ReminderThread.ReoccuringEvents
import Javran.WhaleChan.ReminderThread.EventReminder

data EReminderSupply =
  forall rs. (ReminderSupply rs, Typeable rs) => ERS (Proxy rs)

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

{-
  a ReminderSupply, when given current time,
  supplies a sorted list of times for the timer thread
 -}
class ReminderSupply (r :: k) where
    renewSupply :: forall p. p r -> TimeZoneSeries -> UTCTime -> Maybe EventReminder
    eventDescription :: forall p. p r -> String

    default eventDescription :: (Typeable r) => p r -> String
    eventDescription p = show (typeRep p)


-- | create a EventReminder with supplied eventTime and a list
--   of due times (in minutes) by which the reminder should be sent out
--   prior to the event time.
--   note that in the resulting list, eventTime is always in the list of
--   dueTimes so you don't have to pass that as argument explicitly.
createEventReminderWithDueList :: UTCTime -> [Int] -> Maybe EventReminder
createEventReminderWithDueList eventTime dueListPre =
    makeEventReminder eventTime  (mkTime <$> dueList)
  where
    -- descending list of time without duplicated elements
    dueList = 0 : dueListPre
    mkTime mins = addUTCTime (fromIntegral @Int $ -60 * mins) eventTime

renewSupplyByFunc ::
  (LocalTime -> LocalTime) -> TimeZoneSeries -> UTCTime -> Maybe EventReminder
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


reminderSupplies :: [EReminderSupply]
reminderSupplies =
    [ ERS (Proxy :: Proxy 'PracticeReset)
    , ERS (Proxy :: Proxy 'DailyQuestReset)
    , ERS (Proxy :: Proxy 'WeeklyQuestReset)
    , ERS (Proxy :: Proxy 'MonthlyQuestReset)
    , ERS (Proxy :: Proxy 'QuarterlyQuestReset)
    , ERS (Proxy :: Proxy 'ExtraOperationReset)
    , ERS (Proxy :: Proxy 'SenkaAccounting)
    , ERS (Proxy :: Proxy 'QuestPointDeadline)
    ]

-- a hack to allow "encoding / decoding" of TypeRep through Show instance
-- for now it's a safe assumption that conversion through Show is consistent
reminders :: [(String, TypeRep)]
reminders = f <$> reminderSupplies
  where
    f (ERS ty) = (show tRep, tRep)
      where
        tRep = typeRep ty

strToReminderTypeRep :: String -> Parser TypeRep
strToReminderTypeRep raw = maybe mzero pure (lookup raw reminders)

{-
checkEventReminder :: EventReminder -> Maybe String
checkEventReminder (EventReminder x xs)
  | null xs = Just "event reminder has empty due list"
  | last xs /= x = Just "event reminder last event not matching occur time"
  | and $ zipWith (<) xs (tail xs) = Nothing
  | otherwise = Just "event reminder not is strict ascending order."
 -}
