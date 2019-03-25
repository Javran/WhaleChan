{-# LANGUAGE DeriveGeneric #-}
module Javran.WhaleChan.ReminderThread.EventReminderNew
  ( EventReminderNew
  , eventOccurTime
  , eventReminderDues
  , makeEventReminderNew
  , getDuesByTime
  ) where

import GHC.Generics
import qualified Data.Set as S
import Data.Time.Clock
import Data.Aeson

{-
  this module is an attempt to hide details of EventReminder
  so that invariants can be enforced in a better way.
 -}

{-
  EventReminder contains info about the time the event will occur
  and a sorted list of times that a reminder is due.
 -}

data EventReminderNew
  = EventReminderNew
  { eventOccurTime :: UTCTime
    {-
      INVARIANT: the list should be sorted and every element unique
      for now we don't put any constraints between eventOccurTime and eventReminderDues.
      as we haven't seen a reason to do so.
     -}
  , eventReminderDues :: [UTCTime]
  } deriving (Show, Eq, Generic)

instance FromJSON EventReminderNew
instance ToJSON EventReminderNew

makeEventReminderNew :: UTCTime -> [UTCTime] -> Maybe EventReminderNew
makeEventReminderNew et erds =
    if null sortedUniqErds
      then Nothing
      else Just $ EventReminderNew et erds
  where
    sortedUniqErds = S.toAscList . S.fromList $ erds

-- like span but on EventReminder, a cut time is used and values less or equal to that
-- will be extracted from the list.
getDuesByTime :: UTCTime -> EventReminderNew -> ([UTCTime], Maybe EventReminderNew)
getDuesByTime cutTime er@(EventReminderNew et erds) =
    (erdsOld, remainingER)
  where
    (erdsOld, erdsNew) = span (<= cutTime) erds
    remainingER =
      if null erdsOld
        then Just er
        else makeEventReminderNew et erdsNew
