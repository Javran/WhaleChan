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

import Javran.WhaleChan.Types
import Javran.WhaleChan.Util

import qualified Javran.WhaleChan.Log as Log

getTwInfo :: WConf -> TWInfo
getTwInfo WConf{..} = TWInfo twTok Nothing
  where
    oauth = twitterOAuth
              { oauthConsumerKey = twConsumerKey
              , oauthConsumerSecret = twConsumerSecret
              }
    credential = Credential
                 [ ("oauth_token", BSC.pack twOAuthToken)
                 , ("oauth_token_secret", BSC.pack twOAuthSecret)
                 ]
    twTok = TWToken oauth credential

callTwApi :: FromJSON respTy
          => String -> APIRequest apiName respTy -> (respTy -> WCM s ()) -> WCM s ()
callTwApi tag req handleResp = do
    (wconf, TCommon{tcManager}) <- ask
    let twInfo = getTwInfo wconf
    respM <- liftIO $ guardHttpException (callWithResponse twInfo tcManager req)
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
          (Just rRem, Just rLim)
            | rRem * 5 < rLim ->
                -- rRem / rLim < 20%=1/5 => 5 * rem < lim
                Log.w tag "rate limit availability < 20%"
          _ -> pure ()
        handleResp responseBody
