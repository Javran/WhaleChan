{-# LANGUAGE LambdaCase, OverloadedStrings, NamedFieldPuns #-}
module Javran.WhaleChan.Env
  ( getTWInfo
  ) where

import System.Environment
import Web.Twitter.Conduit.Types
import Data.Default
import Web.Authenticate.OAuth
import Data.String

getOrDie :: IsString s => String -> IO s
getOrDie vName = lookupEnv vName >>= \case
    Nothing -> err
    Just [] -> err
    Just x -> pure (fromString x)
  where
    err = fail $ "Error while getting ENV: " <> vName

getTWInfo :: IO TWInfo
getTWInfo = do
    oauthConsumerKey <- getOrDie "CONSUMER_KEY"
    oauthConsumerSecret <- getOrDie "CONSUMER_SECRET"
    let twOAuth = twitterOAuth {oauthConsumerKey, oauthConsumerSecret}
    oToken <- getOrDie "OAUTH_TOKEN"
    oTokenSecret <- getOrDie "OAUTH_TOKEN_SECRET"
    let twCredential = Credential [("oauth_token", oToken), ("oauth_token_secret", oTokenSecret)]
    let twToken = TWToken {twOAuth, twCredential}
    pure $ def {twToken}
