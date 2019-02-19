module Javran.WhaleChan.FromSource.TimeFormat where

import Data.Time.LocalTime (TimeZone(..), ZonedTime(..))
import Control.Monad.Fail
import Data.Time.Format

jst :: TimeZone
jst = TimeZone (9 * 60) False "JST"

-- default locale with JST included
timeLocale :: TimeLocale
timeLocale = defaultTimeLocale
    { knownTimeZones = jst : knownTimeZones defaultTimeLocale }

fmtKcsConst, fmtKc3Kai, fmtWikia, fmtKcwiki :: String
fmtKcsConst = "%Y/%m/%d %T"
fmtKc3Kai = "%a, %d %B %Y %T %z"
fmtWikia = "%B %-d %Y %T %z"
fmtKcwiki = "%Y/%-m/%-d %T %z"

mkParser :: MonadFail m => String -> String -> m ZonedTime
mkParser = parseTimeM True timeLocale

test :: IO ()
test = do
  let t fs r = mkParser fs r >>= print
  t fmtKcsConst "2019/02/08 11:00:00"
  t fmtKcsConst "2019/02/08 20:25:00"
  t fmtKc3Kai "Fri, 08 February 2019 11:00:00 +0900"
  t fmtKc3Kai" Fri, 08 February 2019 21:00:00 +0900"
  t fmtWikia "February 27 2019 11:00:00 +0900"
  t fmtWikia "February 27 2019 19:00:00 +0900"
  t fmtKcwiki  "2019/2/27 11:00:00 +0900"
