{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , ScopedTypeVariables
  , TypeApplications
  , FlexibleContexts
  #-}
module Javran.WhaleChan.Twitter where

import Control.Monad.RWS
import Control.Exception
import Data.Aeson
import Web.Twitter.Conduit hiding (count)

import qualified Data.ByteString.Char8 as BSC

import Network.HTTP.Client
import Javran.WhaleChan.Types

import qualified Javran.WhaleChan.Log as Log

{-
  this module contains utils relatd to twitter-api
 -}

{-
  Regarding rate limit:

  - we plan to call twitter api:

    + every 5 seconds for profile info
    + every 3 seconds for tweet update

  - this means every minute should have at most 32 calls

  - standard rate limit for our purpose is 900 calls in a 15 minute window,
    so at any moment in time, expect # of remaining calls to be >= 420 (420/900=46.67%)

  - TODO turns out rate limit are applied on a per-api basis, we can have a better interval.

  - we'll send a warning when:

    + either remaining call goes below 400
    + or remaining / limit (= 900 for now) goes below 20%

 -}

getTwInfo :: WConf -> TWInfo
getTwInfo WConf{..} = TWInfo twTok Nothing
  where
    oauth =
      twitterOAuth
      { oauthConsumerKey = twConsumerKey
      , oauthConsumerSecret = twConsumerSecret
      }
    credential =
      Credential
      [ ("oauth_token", BSC.pack twOAuthToken)
      , ("oauth_token_secret", BSC.pack twOAuthSecret)
      ]
    twTok = TWToken oauth credential

callTwApi :: FromJSON respTy
          => String -> APIRequest apiName respTy -> (respTy -> WCM s ()) -> WCM s ()
callTwApi tag req handleResp = do
    (wconf, TCommon{tcManager}) <- ask
    let twInfo = getTwInfo wconf
    respM <- liftIO $
      (Right <$> callWithResponse twInfo tcManager req) `catches`
        {-
          network issue and twitter api issue can be temporary,
          so we capture these two kinds of erros instead of
          allowing them to throw
         -}
        [ Handler $ \(e :: HttpException) -> (pure . Left . toException) e
        , Handler $ \(e :: TwitterError) -> (pure . Left . toException) e
        ]
    case respM of
      Left e ->
        -- a network exception could be temporary, so
        -- we'll let it proceed instead of throwing exceptions
        Log.e tag (displayException e)
      Right Response{responseHeaders, responseBody} -> do
        let [rlLimit,rlRemaining,_rlReset] =
              ((read @Int . BSC.unpack) <$>) . (`Prelude.lookup` responseHeaders) <$>
                [ "x-rate-limit-limit"
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
          _ -> do
            Log.w tag "rate limit header not available"
            Log.w tag $ "the request was" <> show req
        handleResp responseBody
