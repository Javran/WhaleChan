{-# LANGUAGE
    LambdaCase
  , TypeApplications
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , OverloadedStrings
  , DeriveGeneric
  #-}
module Javran.WhaleChan.TimerThread
 ( reminderThread
 ) where

import Control.Arrow
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Coerce
import Data.Default
import Data.Function
import Data.List
import Data.List.Ordered
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import Data.Typeable
import GHC.Generics
import Say
import Web.Telegram.API.Bot

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as TB

import Javran.WhaleChan.Types
import Javran.WhaleChan.Base
import Javran.WhaleChan.ReminderSupply
import Javran.WhaleChan.Util

{-
  reminder impl draft:

  - concepts:

    + a EventReminder contains a sorted list of pending reminders
      around a particular event.
    + several type implements ReminderSupply, which, when called with
      current time generates a corresponding EventReminder.
      (NOTE: this is a pure function, we'll worry about parsing maintenance times later)
    + these types are promoted data types, unified by a common kind: ReminderSupplier

- we'll have a list of reminders implemented, visible to
  the reminder thread

- the timer thread keeps track of a Map from TypeRep of ReminderSupplier
  to EventReminder

- when EventReminder cannot be found or contains an empty list of eventReminderDues,
  timer thread will attempt to "resupply" by calling corresponding reminder.

- we'll basically reminder the following events:

    + "30 mins before [some event]"
    + "10 mins before [some event]"
    + "5 mins before [some event]"
    + "[some event] is happening"

  but this is by no meaning a limit, the definition of ReminderSupply
  is flexible to allow time much more longer - for example

- the reminder thread is a loop that:

  + wakes up at (roughly) the beginning of every minute
  + then determine if it's time to remind something
  + send the post to telegram thread
  + after this is done, we'll try to "resupply" EventReminder that are empty
  + for dealing with maintenance times, we'll have a dummy ReminderSupplier
    that does nothing upon resupply - a dedicated thread will check and parse
    maintenance time on a regular basis and supply timer thread with result,
    instead of doing passive "resupply"

- TODO we current have the problem that when one reminder coincide with
  another one generated by the same event reminder, the latter gets one minute delay.
  this is because we only renew after messages are processed, we'll need some thoughts
  to get this processed properly.

- However, this is not a big deal though, as short-term reminder suppliers
  like "practice refresh" or "daily quest reset" are not supposed
  to have reminders that are "12 hours before" and other longer events
  like weekly or quarterly won't have this overlapping problem.

- Draft of a possible fix:
  change State type to Map TypeRep [EventReminder]
  in which EventReminders are sorted and have unique eventOccurTime

  + now that ReminderSupply will be used everytime we wake up to see if we can
    an additional EventReminder is needed (obviuosly if we end up with same eventOccurTime
    we should not do anything to the existing EventReminder)

 -}

oneSec :: Int
oneSec = 1000000

oneMin :: Int
oneMin = oneSec * 60

{-
  wait and wake up at (roughly) begining of the next minute
  -- https://stackoverflow.com/a/8578237/315302
 -}
waitUntilStartOfNextMinute :: IO ()
waitUntilStartOfNextMinute = do
    t <- getCurrentTime
    -- compute millseconds since beginning of current minute
    let ms = round (fromIntegral oneSec * realToFrac @_ @Double (utctDayTime t)) `rem` oneMin
    -- wait to start of next minute
    threadDelay $ oneMin - ms
    -- TODO: we only wait for 5s now for it's easier to debug
    -- threadDelay (oneSec * 5)

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

type TimerM = StateT (M.Map TypeRep EventReminder) IO

{-
  keeping old time message in case needed
  sayString $ "  Remaining time: " <> timeStr
  sayString $ "  Japan:   " <> show (localDay lt) <> " " <> show (localTimeOfDay lt)
  sayString $ "  Pacific: " <> show (localDay lt') <> " " <> show (localTimeOfDay lt')
  let tgMsg = "Reminder: " <> T.pack (show tyRep) <> " Remaining time: " <> T.pack timeStr
 -}

-- TODO: use lens-datetime

{-
  note that [EventReminder] is sorted in time order,
  and is supposed to have no more than 2 items - as reminders are restocked at that
  exact moment, there should be one passing (0 seconds) and new one being added.
  we are still under the assumption that no more than 2 reminders (with beforhand reminders)
  will happen at the same time, which is quite safe considering the nature of this game
  (i.e. frequent events shouldn't be reminded too often (< 24 hours) and less frequent
  will have a relatively large interval between them, large enough that the overlapping
  of beforehand reminds are very unlikely.
 -}
newtype ReminderDict
  = RD {getRD :: M.Map TypeRep [EventReminder] }
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

type ReminderM = WCM ReminderDict

reminderThread :: WEnv -> IO ()
reminderThread wenv = do
    let cv :: forall a. WCM (M.Map TypeRep [EventReminder]) a -> ReminderM a
        cv = coerce -- to avoid the noise introduced by newtype
        (_, TCommon{tcTelegram}) = wenv
    -- load tz info before starting the loop
    _tzPt <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/US/Pacific"
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/Asia/Tokyo"
    waitUntilStartOfNextMinute
    autoWCM @ReminderDict "Reminder" "reminder.yaml" wenv $ \markStart' -> cv $ do
      let markStart = coerce markStart' ::
            WCM (M.Map TypeRep [EventReminder]) (WCM (M.Map TypeRep [EventReminder]) ())
      -- note that unlike other threads, this one begins by thread sleep
      -- the idea is to start working immediately after wake up
      -- so we get the most accurate timestamp to work with
      curTime <- liftIO (waitUntilStartOfNextMinute >> getCurrentTime)
      -- any event in future within 20 seconds is also include
      -- this assume that we can always process all reminders within 20 seconds
      -- which should be way more than enough.
      let tThres = addUTCTime 20 curTime
      markEnd <- markStart
      let collectResults :: Endo [(EReminderSupply, UTCTime)] -> [(EReminderSupply, [UTCTime])]
          collectResults xsPre = convert <$> ys
            where
              ersToTyRep (ERS tp) = typeRep tp
              xs = appEndo xsPre []
              ys = groupBy ((==) `on` ersToTyRep . fst) xs
              convert ts = (fst . head $ ts, snd <$> ts)
      -- scan through supplies and try restocking if an new event timestamp is found
      displayList <- collectResults <$>
        execWriterT (forM reminderSupplies (\e@(ERS tp) -> do
            let tyRep = typeRep tp
                -- always compute new supply (lazily)
                newSupply = renewSupply tp tzs curTime
                cmp = comparing eventOccurTime
            mValue <- gets (M.lookup tyRep)
            -- restock step
            case mValue of
              Nothing ->
                modify (M.insert tyRep [newSupply])
              Just ers ->
                -- skip restocking if timestamp mismatches
                unless (hasBy cmp ers newSupply) $
                  modify (M.adjust (insertSetBy cmp newSupply) tyRep)
            eventReminders <- fromMaybe [] <$> gets (M.lookup tyRep)
            newEventReminders <-
              catMaybes <$> forM eventReminders (\(EventReminder eot erds) -> do
                -- if remindsDue contains anything, we should send current reminder
                let (remindsDue, erds') = span (< tThres) erds
                unless (null remindsDue) $ tell . Endo $ ([(e, eot)] ++)
                pure $ if null erds'
                    then Nothing
                    else Just (EventReminder eot erds')
              )
            let newVal =
                  if null newEventReminders
                    then Nothing
                    else Just newEventReminders
            modify (M.update (const newVal) tyRep)
        ))
      markEnd
      let txt = toStrict (TB.toLazyText md)
          md = foldMap pprERS displayList
          pprERS (ERS tp, eTimes) =
              "- Reminder: **" <> TB.fromString (show tyRep) <> "**\n" <>
                foldMap pprTime eTimes
            where
              tyRep = typeRep tp
              pprTime eTime =
                  "    + " <> TB.fromString timeStr <> "\n"
                where
                  timeStr = describeDuration (round (eTime `diffUTCTime` curTime) :: Int)
      void $ liftIO $ writeChan tcTelegram (TgRMTimer txt (Just Markdown))
