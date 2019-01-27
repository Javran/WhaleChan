{-# LANGUAGE
    TypeApplications
  , NamedFieldPuns
  #-}

{-
  this module implements a bunch of nextXXXX functions
  corresponding to the game.

  - all times are in JST
  - the input is current time
  - the output is next time that the event happens
  - note that we consider input time to be in the past
    at the time that this function is called,
    which means input should never be the same as output:
    say if you call nextDailyQuestReset with exact reseting time 5:00 JST,
    today's 5:00 JST is already a "past" so you'll get 5:00 JST of next day back instead

 -}

module Javran.WhaleChan.ReoccuringEvents
  ( nextPracticeReset
  , nextDailyQuestReset
  , nextWeeklyQuestReset
  , nextMonthlyQuestReset
  , nextQuarterlyQuestReset
  , nextExtraOperationReset
  , nextSenkaAccounting
  , nextQuestPointDeadline
  ) where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime

localDayAdd :: Int -> LocalTime -> LocalTime
localDayAdd n lt@LocalTime{localDay = ld} = lt {localDay = addDays (fromIntegral n) ld}

nextPracticeReset :: LocalTime -> LocalTime
nextPracticeReset lt@LocalTime{localTimeOfDay = TimeOfDay {todHour}}
  | todHour < 3 = todayAt3
  | todHour < 15 = todayAt15
  | otherwise = localDayAdd 1 todayAt3
  where
    todayAt3 = lt {localTimeOfDay = TimeOfDay 3 0 0}
    todayAt15 = lt {localTimeOfDay = TimeOfDay 15 0 0}

nextDailyQuestReset :: LocalTime -> LocalTime
nextDailyQuestReset lt@LocalTime{localTimeOfDay = TimeOfDay {todHour}}
  | todHour < 5 = todayAt5
  | otherwise = localDayAdd 1 todayAt5
  where
    todayAt5 = lt {localTimeOfDay = TimeOfDay 5 0 0}

nextWeeklyQuestReset :: LocalTime -> LocalTime
nextWeeklyQuestReset lt@LocalTime{localDay, localTimeOfDay = TimeOfDay {todHour}}
  | dayOfWeek /= 1 = nextMondayAt5
  | todHour < 5 = todayAt5
  | otherwise = nextMondayAt5
  where
    todayAt5 = lt {localTimeOfDay = TimeOfDay 5 0 0}
    nextMondayAt5 = localDayAdd (8 - dayOfWeek) todayAt5
    (_, _, dayOfWeek) = toWeekDate localDay

nextMonthlyQuestReset :: LocalTime -> LocalTime
nextMonthlyQuestReset LocalTime{localDay, localTimeOfDay = TimeOfDay {todHour}}
  | day /= 1 = nextFirstDayAt5
  | todHour < 5 = firstDayAt5
  | otherwise = nextFirstDayAt5
  where
    (year, month, day) = toGregorian localDay
    at5 = TimeOfDay 5 0 0
    firstDay = fromGregorian year month 1
    firstDayAt5 = LocalTime firstDay at5
    nextFirstDayAt5 = LocalTime (addGregorianMonthsClip 1 firstDay) at5

nextQuarterlyQuestReset :: LocalTime -> LocalTime
nextQuarterlyQuestReset LocalTime{localDay, localTimeOfDay = TimeOfDay {todHour}}
  | month /= nextResetMonth = resetMonthFirstDayAt5
  | day == 1 && todHour < 5 = resetMonthFirstDayAt5
  | otherwise = nextQuarterFirstDayAt5
  where
    (year, month, day) = toGregorian localDay
    nextResetMonth = 3 * ceiling @Double @Int (fromIntegral month / 3)
    at5 = TimeOfDay 5 0 0
    resetMonthFirstDayAt5 =
      LocalTime (fromGregorian year nextResetMonth 1) at5
    nextQuarterFirstDayAt5 =
      LocalTime (addGregorianMonthsClip 3 $ fromGregorian year nextResetMonth 1) at5

nextExtraOperationReset :: LocalTime -> LocalTime
nextExtraOperationReset LocalTime{localDay} = LocalTime lDay (TimeOfDay 0 0 0)
  where
    (year, month, _) = toGregorian localDay
    lDay = addGregorianMonthsClip 1 (fromGregorian year month 1)

{-
  usually 2:00 JST and 14:00 JST, but for the last day of a month, there's an additional 22:00 JST
 -}
nextSenkaAccounting :: LocalTime -> LocalTime
nextSenkaAccounting lt@LocalTime{localTimeOfDay = TimeOfDay {todHour}}
  | todHour < 2 = todayAt2
  | todHour < 14 = lt { localTimeOfDay = TimeOfDay 14 0 0 }
  | day == lastDay && todHour < 22 = lt { localTimeOfDay = TimeOfDay 22 0 0 }
  | otherwise =
    -- next day at 22
    LocalTime (addDays 1 (localDay todayAt2)) (TimeOfDay 2 0 0)
  where
    todayAt2 = lt { localTimeOfDay = TimeOfDay 2 0 0 }
    (year, month, day) = toGregorian (localDay lt)
    lastDay = gregorianMonthLength year month

nextQuestPointDeadline :: LocalTime -> LocalTime
nextQuestPointDeadline lt@LocalTime{localTimeOfDay = TimeOfDay {todHour}}
  | day /= lastDay = thisMonthDdl
  | todHour < 14 = thisMonthDdl
  | otherwise =
      -- compute as if we are in first day of next month
      -- given that this will always satisfy day /= lastDay, we are safe from infinite loops
      nextQuestPointDeadline $
        LocalTime
          (addGregorianMonthsClip 1 (fromGregorian year month 1))
          (TimeOfDay 0 0 0)
  where
    thisMonthDdl = LocalTime (fromGregorian year month lastDay) (TimeOfDay 14 0 0)
    (year, month, day) = toGregorian (localDay lt)
    lastDay = gregorianMonthLength year month
