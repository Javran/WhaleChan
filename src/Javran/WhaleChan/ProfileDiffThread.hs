{-# LANGUAGE
    DeriveGeneric
  , TypeApplications
  , NamedFieldPuns
  , RecordWildCards
  , OverloadedStrings
  #-}
module Javran.WhaleChan.ProfileDiffThread where

import Web.Twitter.Types
import Data.Time.Clock
import GHC.Generics
import Data.Aeson
import Data.Default
import Control.Concurrent
import Control.Monad.RWS
-- import Control.Exception
import Control.Lens
import Web.Twitter.Conduit (usersShow)
import Web.Twitter.Conduit.Parameters
import qualified Data.Text as T
import Network.HTTP.Client

import Javran.WhaleChan.Types
import Javran.WhaleChan.Base
import Javran.WhaleChan.Util
import Javran.WhaleChan.Twitter
import qualified Javran.WhaleChan.Log as Log
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

{-
  (draft)
  This thread detects profile image change
  and also sends a daily message about changes of:

  - status count
  - following count
  - follower count

  also let's say the daily message is sent around 00:00 JST.
 -}

data ProfileStat
  = ProfileStat
  { statusesCount :: Int
  , friendsCount :: Int
  , followersCount :: Int
  } deriving (Show, Generic, Eq)

instance FromJSON ProfileStat
instance ToJSON ProfileStat

data ProfileInfo
  = ProfileInfo
  { lastProfileImage :: Maybe URIString
  , lastStat :: Maybe (UTCTime, ProfileStat)
  } deriving (Show, Generic, Eq)

instance Default ProfileInfo
instance FromJSON ProfileInfo
instance ToJSON ProfileInfo

type M = WCM ProfileInfo

-- ref: https://developer.twitter.com/en/docs/accounts-and-users/user-profile-images-and-banners
normalSuffix :: T.Text
normalSuffix = "_normal.png"

getOriginalProfileUrl :: URIString -> Maybe URIString
getOriginalProfileUrl normUrl = do
    guard $ normalSuffix `T.isSuffixOf` normUrl
    let base = T.dropEnd (T.length normalSuffix) normUrl
    pure (base <> ".png")

extractInfo :: User -> (Maybe URIString, ProfileStat)
extractInfo User{..} =
    ( userProfileImageURLHttps >>= getOriginalProfileUrl
    , ProfileStat
        userStatusesCount
        userFriendsCount
        userFollowersCount
    )

fetchImg :: URIString -> WCM s (Either HttpException BS.ByteString)
fetchImg uri = do
  (_, TCommon{tcManager=mgr}) <- ask
  z <- liftIO $ guardHttpException $ do
    req <- parseRequest (T.unpack uri)
    resp <- httpLbs req mgr
    pure (responseBody resp)
  pure (BSL.toStrict <$> z)

profileDiffThread :: WEnv -> IO ()
profileDiffThread wenv = do
    let (WConf{twWatchingUserId},TCommon{tcTelegram}) = wenv
        req = usersShow (UserIdParam (fromIntegral twWatchingUserId))
              & includeEntities ?~ False
        tag = "ProfileDiff"
    autoWCM @ProfileInfo tag "profile-diff.yaml" wenv $ \markStart -> do
        markEnd <- markStart
        callTwApi "ProfileDiff" req $ \userInfo -> do
            let (mNewUrl, _) = extractInfo userInfo
            case mNewUrl of
              Nothing ->
                Log.w tag "url parsing error or account has no img url"
              Just newUrl -> do
                ProfileInfo curUrl _ <- get
                when (curUrl /= mNewUrl) $ do
                  Log.i tag "new img detected"
                  liftIO $ writeChan tcTelegram (TgRMProfileImg newUrl)
                  modify (\s -> s {lastProfileImage = mNewUrl})
                  {- mImgData <- fetchImg newUrl
                  case mImgData of
                    Left e -> Log.e tag (displayException e)
                    Right imgData -> do
                      liftIO $ writeChan tcTelegram (TgRMProfileImg imgData)
                      -- only update state when url fetch is successful.
                      modify (\s -> s {lastProfileImage = mNewUrl})
                   -}
        markEnd
        liftIO $ threadDelay $ 2 * oneSec
