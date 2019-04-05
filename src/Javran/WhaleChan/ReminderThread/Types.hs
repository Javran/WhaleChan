{-# LANGUAGE
    TypeApplications
  , DataKinds
  , ExistentialQuantification
  , PolyKinds
  , DefaultSignatures
  , DeriveGeneric
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
  , MaintenanceEventReminder
  , ReminderM, ReminderM'
  , ReminderState, ReminderState'
  , ReminderDict(..)
  ) where

import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series
import Data.Typeable
import GHC.Generics

import qualified Data.Map as M

import Javran.WhaleChan.ReminderThread.ReoccuringEvents
import Javran.WhaleChan.ReminderThread.EventReminder
import Javran.WhaleChan.Types

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
    makeEventReminder eventTime (mkTime <$> dueList)
  where
    dueList = 0 : dueListPre
    mkTime mins = addUTCTime (fromIntegral @Int $ -60 * mins) eventTime

renewSupplyByFunc
  :: (LocalTime -> LocalTime)
  -> [Int]
  -> TimeZoneSeries
  -> UTCTime
  -> Maybe EventReminder
renewSupplyByFunc getNextTime xs tzs ut =
    createEventReminderWithDueList eventTime xs
  where
    eventTime :: UTCTime
    eventTime =
      localTimeToUTC' tzs
      . getNextTime
      . utcToLocalTime' tzs
      $ ut

instance ReminderSupply 'PracticeReset where
    renewSupply _ =
      renewSupplyByFunc
        nextPracticeReset
        [24*60, 30, 10, 5]
    eventDescription _ = "Practice Reset"

instance ReminderSupply 'DailyQuestReset where
    renewSupply _ =
      renewSupplyByFunc
        nextDailyQuestReset
        [24*60, 30, 10, 5]
    eventDescription _ = "Daily Quest Reset"

instance ReminderSupply 'WeeklyQuestReset where
    renewSupply _ =
      renewSupplyByFunc
        nextWeeklyQuestReset
        [24*60, 30, 10, 5]
    eventDescription _ = "Weekly Quest Reset"

instance ReminderSupply 'MonthlyQuestReset where
    renewSupply _ =
      renewSupplyByFunc
        nextMonthlyQuestReset
        [24*60, 30, 10, 5]
    eventDescription _ = "Monthly Quest Reset"

instance ReminderSupply 'QuarterlyQuestReset where
    renewSupply _ =
      renewSupplyByFunc
        nextQuarterlyQuestReset
        [24*60, 30, 10, 5]
    eventDescription _ = "Quarterly Quest Reset"

instance ReminderSupply 'ExtraOperationReset where
    renewSupply _ =
      renewSupplyByFunc
        nextExtraOperationReset
        [24*60, 30, 10, 5]
    eventDescription _ = "Extra Operation Reset"

instance ReminderSupply 'SenkaAccounting where
    renewSupply _ =
      renewSupplyByFunc
        nextSenkaAccounting
        [24*60, 30, 10, 5]
    eventDescription _ = "Senka Accounting"

instance ReminderSupply 'QuestPointDeadline where
    renewSupply _ =
      renewSupplyByFunc
        nextQuestPointDeadline
        [24*60, 30, 10, 5]
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
  note that [EventReminder] is sorted in time order,
  and is supposed to have no more than 2 items - as reminders are restocked at that
  exact moment, there should be one passing (0 seconds) and new one being added.
  we are still under the assumption that no more than 2 reminders (with beforhand reminders)
  will happen at the same time, which is quite safe considering the nature of this game
  (i.e. frequent events shouldn't be reminded too often (< 24 hours) and less frequent
  will have a relatively large interval between them, large enough that the overlapping
  of beforehand reminds are very unlikely.)
 -}
type ReminderMap = M.Map TypeRep [EventReminder]

newtype ReminderDict
  = RD { getRD :: ReminderMap }
  deriving (Eq, Generic)

instance Default ReminderDict

instance ToJSON ReminderDict where
  toJSON (RD d) = toJSON @[(String,[EventReminder])] (first show <$> M.toList d)

instance FromJSON ReminderDict where
  parseJSON o =
      RD . M.fromList <$>
        (parseJSON @[(String, [EventReminder])] o >>= mapM convert)
    where
      convert :: (String, [EventReminder]) -> Parser (TypeRep, [EventReminder])
      convert (r, er) = (,) <$> strToReminderTypeRep r <*> pure er

type MaintenanceEventReminder =
  ( Maybe (EventReminder, [String]) -- INVARIANT: we'll keep sources sorted by `sort`
  , Maybe (EventReminder, [String]) -- same INVARIANT
  )

-- the ' version is actual resentation without newtype wrappers
type ReminderState = (ReminderDict, MaintenanceEventReminder)
type ReminderState' = (ReminderMap, MaintenanceEventReminder)
type ReminderM = WCM ReminderState
type ReminderM' = WCM ReminderState'
