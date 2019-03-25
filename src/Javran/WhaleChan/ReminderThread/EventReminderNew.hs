module Javran.WhaleChan.ReminderThread.EventReminderNew
  ( EventReminderNew
  , eventOccurTime
  , eventReminderDues
  , makeEventReminderNew
  ) where

import qualified Data.Set as S
import Data.Time.Clock

{-
  this module is an attempt to hide details of EventReminder
  so that invariants can be enforced in a better way.
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
  }

makeEventReminderNew :: UTCTime -> [UTCTime] -> Maybe EventReminderNew
makeEventReminderNew et erds =
    if null sortedUniqErds
      then Nothing
      else Just $ EventReminderNew et erds
  where
    sortedUniqErds = S.toAscList . S.fromList $ erds
