module Javran.WhaleChan.FromSource.TimeFormat
  ( jst
  , timeLocale
  , mkTimeParser
  , ZonedTime(..)
  , PRange
  , toPRange
  , UTCTime(..)
  , eitherToMaybe
  , mkUtcInJst
  ) where

import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

import Javran.WhaleChan.Util
import Javran.WhaleChan.Types

jst :: TimeZone
jst = TimeZone (9 * 60) False "JST"

-- default locale with JST included
timeLocale :: TimeLocale
timeLocale = defaultTimeLocale
    { knownTimeZones = jst : knownTimeZones defaultTimeLocale }

mkUtcInJst :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
mkUtcInJst year month day hh mm ss =
    zonedTimeToUTC $
      ZonedTime
        (LocalTime
          (fromGregorian year month day)
          (TimeOfDay hh mm (fromIntegral ss)))
        jst

{-
  we are not using m ~ (Either String) here as it calls
  "Monad.fail" instead of "Monad.Fail.fail", the result is unsafe.
 -}
mkTimeParser :: ParseTime t => String -> String -> Either String t
mkTimeParser fmt inp = case readSTime True timeLocale fmt inp of
    [] -> Left "no parse"
    [(x,"")] -> Right x
    [(_,ys@(_:_))] -> Left $ "leftover found: " ++ ys
    _:_ -> Left "multiple parses found"
