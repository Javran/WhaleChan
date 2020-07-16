{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.WhaleChan.Twitter
  ( callTwApi
  , callTwApi'
  , createTweetLinkMarkdown
  , statusGetTweetUrls
  , TweetUrls (..)
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad.RWS
import Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import Data.Conduit.Attoparsec
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Data.Time.Format
import Data.Time.LocalTime.TimeZone.Series
import qualified Javran.WhaleChan.Log as Log
import Javran.WhaleChan.Types
import Javran.WhaleChan.Util
import Network.HTTP.Client
import Web.Twitter.Conduit
import Web.Twitter.Conduit.Base (ResponseBodyType)
import Web.Twitter.Types

{-
  this module contains utils relatd to twitter-api
 -}

{-
  Regarding rate limit:

  - we plan to call twitter api:

    + every 2 seconds for profile info
    + every 3 seconds for tweet update

  - this means every minute should have at most 30 calls (for each API)

  - standard rate limit for our purpose is 900 calls in a 15 minute window,
    so at any moment in time, expect # of remaining calls to be >= 450 (450/900=50%)

  - we'll send a warning when:

    + either remaining call goes below 400
    + or remaining / limit (= 900 for now) goes below 20%

 -}

callTwApi'
  :: (FromJSON respTy, ResponseBodyType respTy)
  => String
  -> APIRequest apiName respTy
  -> r
  -> (Either TwitterError respTy -> WCM s r)
  -> WCM s r
callTwApi' tag req fallback handleResp' = do
  (WConf {twInfo}, TCommon {tcManager}) <- ask
  respM <-
    liftIO $
      (Right <$> callWithResponse twInfo tcManager req)
        `catches`
        {-
          network issue and twitter api issue can be temporary,
          so we capture these two kinds of erros instead of
          allowing them to throw
         -}
        [ Handler $ \(e :: HttpException) -> (pure . Left . toException) e
        , Handler $ \(e :: TwitterError) -> (pure . Left . toException) e
        , {-
            occasionally a ParseError could show up and bring down the whole thread,
            but in our case it's just like a minor API issue so we choose to
            ignore and move on
           -}
          Handler $ \(e :: ParseError) -> (pure . Left . toException) e
        , Handler $ \(e :: IOException) ->
            if isConnectionResetException e
              then (pure . Left . toException) e
              else throw e
        ]
  case respM of
    Left e -> do
      case fromException @TwitterError e of
        Nothing -> do
          -- we are dealing with a caught exception that is not TwitterError,
          -- in this case we will just log it and let the control proceed.
          Log.e tag (displayExceptionShort e)
          pure fallback
        Just te -> handleResp' (Left te)
    Right Response {responseHeaders, responseBody} -> do
      let [rlLimit, rlRemaining, _rlReset] =
            ((read @Int . BSC.unpack) <$>) . (`Prelude.lookup` responseHeaders)
              <$> [ "x-rate-limit-limit"
                  , "x-rate-limit-remaining"
                  , "x-rate-limit-reset"
                  ]
      case (rlRemaining, rlLimit) of
        (Just rRem, Just rLim) -> do
          -- rRem / rLim < 20%=1/5 => 5 * rem < lim
          when (rRem * 5 < rLim) $
            Log.w tag "rate limit availability < 20%"
          when (rRem < 400) $
            Log.w tag "remaining # of calls < 400"
        _ ->
          {-
            from https://developer.twitter.com/en/docs/basics/rate-limiting:
              "... there may be times when the rate limit values that
               are returned are inconsistent, or cases where no headers
               are returned at all."
            so we consider the case where rate limit header is missing normal,
            and just log it as info.
           -}
          Log.i tag "rate limit header not available"
      handleResp' (Right responseBody)

callTwApi
  :: (FromJSON respTy, ResponseBodyType respTy)
  => String
  -> APIRequest apiName respTy
  -> (respTy -> WCM s ())
  -> WCM s ()
callTwApi tag req handleResp =
  callTwApi'
    tag
    req
    ()
    (\case
       Left _ -> pure ()
       Right r -> handleResp r)

{-
  format: [<Month> <Day>, <Year> at <HH>:<MM> JST](https://twitter.com/<user>/status/<id>)
 -}
createTweetLinkMarkdown :: TimeZoneSeries -> Status -> T.Text
createTweetLinkMarkdown tzTokyo st =
  buildStrictText $
    "[" <> TB.fromString timeStr <> "](" <> twUrl <> ")"
  where
    Status
      { statusCreatedAt = stTime
      , statusId = sId
      , statusUser =
        User
          { userScreenName = uScrName
          }
      } = st
    twUrl = "https://twitter.com/" <> TB.fromText uScrName <> "/status/" <> TB.decimal sId
    jstLocalTime = utcToLocalTime' tzTokyo stTime
    timeStr =
      formatTime
        defaultTimeLocale
        "%B %d, %Y at %R JST"
        jstLocalTime

{-
  since extended_entities is more precise and is the recommended way of dealing with
  media objects, we can have this getter to pick the most precise info for us,
  all use of statusEntities and statusExtendedEntities should be using this
  function from now on.

  ref:
  https://developer.twitter.com/en/docs/tweets/data-dictionary/overview/extended-entities-object
 -}

data TweetUrls = TweetUrls
  { mediaUrls :: [URIString]
  , otherUrls :: [URIString]
  }

statusGetTweetUrls :: Status -> Maybe TweetUrls
statusGetTweetUrls st = tryExtended <|> tryCompat
  where
    tryExtended = do
      ExtendedEntities exs <- statusExtendedEntities st
      let urls = fmap (exeMediaUrlHttps . entityBody) exs
      pure $ TweetUrls urls []
    tryCompat = do
      Entities {enURLs = ens, enMedia = ems} <- statusEntities st
      let mediaUrls = fmap (meMediaURLHttps . entityBody) ems
          otherUrls = fmap (ueExpanded . entityBody) ens
      pure $ TweetUrls mediaUrls otherUrls
