{-# LANGUAGE
    OverloadedStrings
  #-}
module Javran.WhaleChan.ReminderThread.Message where

import Data.List
import Data.Time.Clock
import Data.Text.Lazy (toStrict)

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.DList as DL

import Javran.WhaleChan.Util

-- Message representation for a reminder message.
type MessageRep =
  DL.DList ( String -- Event description (e.g. "Daily Quest Reset")
           , [ ( UTCTime -- Event occur time
               , [String] -- sources, only used by maintenance time reminders
               )
             ]
           )

renderMessage :: UTCTime -> MessageRep -> Maybe T.Text
renderMessage curTime xsD =
    toStrict . TB.toLazyText <$>
      case filter (not . null . snd) . DL.toList $ xsD of
        [] -> Nothing
        [(eDesc, [eTimeSrc])] ->
          {-
            when a single line is good enough for displaying
            example:
            > [Reminder] {some event}: {relative time}
           -}
          Just $ tag <> " " <> TB.fromString eDesc <> ": " <> renderTimeSrc eTimeSrc
        [(eDesc, eTimeSrcs)] ->
          {-
            we still have one single event but multiple reminders
            > [Reminder] {some event}:
            > + {relative time}
            > + {relative time}
           -}
          Just $
            tag <> " " <> TB.fromString eDesc <> ":\n"
            <> foldMap (\p -> "+ " <> renderTimeSrc p <> "\n") eTimeSrcs
        ys ->
          {-
            most general case
            > [Reminder]
            > - {some event}: {relative time}
            > - {some other event}:
            >     + {relative time}
            >     + {relative time}
           -}
          Just $
            tag <> "\n" <> foldMap pprBlock ys
  where
    renderTimeSrc (eTime, srcs) =
        timePart <> if null srcs then "" else " " <> srcPart
      where
        timePart =
          TB.fromString $ describeDuration (round (eTime `diffUTCTime` curTime) :: Int)
        srcPart = "(source: " <> TB.fromString (intercalate ", " srcs) <> ")"

    tag = "\\[Reminder]" :: TB.Builder
    pprBlock (eDesc, [eTimeSrc]) =
        "- " <> TB.fromString eDesc <> ": " <> renderTimeSrc eTimeSrc <> "\n"
    pprBlock (eDesc, eTimeSrcs) =
        "- " <> TB.fromString eDesc <> ":\n"
        <> foldMap (\p -> "    + " <> renderTimeSrc p <> "\n") eTimeSrcs

