{-# LANGUAGE
    DeriveGeneric
  , TypeApplications
  , NamedFieldPuns
  , RecordWildCards
  , OverloadedStrings
  #-}
module Javran.WhaleChan.ProfileDiffThread
  ( profileDiffThread
  , fetchImg
  ) where

import Control.Concurrent
import Control.Lens
import Control.Monad.RWS
import Data.Aeson
import Data.Char
import Data.Default
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series
import GHC.Generics
import Network.HTTP.Client
import Web.Twitter.Conduit (usersShow)
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Javran.WhaleChan.Log as Log
import qualified Text.ParserCombinators.ReadP as P

import Javran.WhaleChan.Types
import Javran.WhaleChan.Base
import Javran.WhaleChan.Util
import Javran.WhaleChan.Twitter
import Javran.WhaleChan.HealthThread (heartbeat)
import Javran.WhaleChan.FromSource.TimeFormat (timeLocale)


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

{-
  ref: https://developer.twitter.com/en/docs/accounts-and-users/user-profile-images-and-banners
  also note that there are non-png formats,
  so we'd better have a solution that works on most of the image formats
 -}
pOriginalProfileUrl :: P.ReadP String
pOriginalProfileUrl = do
    xs <- P.many1 P.get
    _ <- P.string "_normal."
    ext <- P.munch1 (\c -> isAscii c && isAlphaNum c)
    P.eof
    pure $ xs <> "." <> ext

getOriginalProfileUrl :: URIString -> Maybe URIString
getOriginalProfileUrl normUrl =
    case P.readP_to_S pOriginalProfileUrl (T.unpack normUrl) of
      [(r,"")] -> Just (T.pack r)
      _ -> Nothing

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
profileDiffThread wenv@(WConf{twWatchingUserId}, TCommon{tzTokyo}) = do
    let req = usersShow (UserIdParam (fromIntegral twWatchingUserId))
              & includeEntities ?~ False
        tag = "ProfileDiff"
        hb = heartbeat "ProfileDiff" 3600 -- kill this thread if it doesn't come back in 1 hr
    autoWCM @ProfileInfo tag "profile-diff.yaml" wenv $ \markStart -> do
        markEnd <- markStart
        hb
        callTwApi "ProfileDiff" req $ \userInfo -> do
            let (mNewUrl, pStat) = extractInfo userInfo
            case mNewUrl of
              Nothing ->
                Log.w tag "url parsing error or account has no img url"
              Just newUrl -> do
                ProfileInfo curUrl _ <- get
                when (curUrl /= mNewUrl) $ do
                  Log.i tag "new img detected"
                  writeToTg (TgRMProfileImg newUrl)
                  modify (\s -> s {lastProfileImage = mNewUrl})
            -- now see if we can update stat, it's still inside
            -- the twitter api so we have access to userInfo
            curTime <- liftIO getCurrentTime
            let tokyoTime = utcToLocalTime' tzTokyo curTime
            let LocalTime _ (TimeOfDay hour _ _) = tokyoTime
            ProfileInfo _ mLastStat <- get
            let shouldSendStatDiff =
                  {-
                     we give it one hour window of opportunity
                     during every midnight hour in JST,
                     this means:
                     - even if network doesn't work for a while,
                       we'll still be posting it, just a bit delayed
                     - if for whatever reason this opportunity is missed,
                       we'll still post the next day, one day of difference is fine.
                   -}
                  hour == 0
                  && maybe
                       -- mLast empty
                       True
                       (\(tLast,_) ->
                          {-
                            1 hour < gap <= 23 hours,
                            anything in between is fine,
                            as it only happens during a specific hour
                           -}
                          curTime `diffUTCTime` tLast >= 3600 * 2)
                       mLastStat
            when shouldSendStatDiff $ do
              let ProfileStat stCnt foCnt fodCnt = pStat
              let (mSince, stDiff, foDiff, fodDiff) = case mLastStat of
                    Nothing -> ("", "", "", "")
                    Just (tLast, ProfileStat stCntPrev foCntPrev fodCntPrev) ->
                      let mkTxt before after =
                            case after `compare` before of
                              GT -> " (+" <> TB.fromString (show $ after - before) <> ")"
                              EQ -> " (0)"
                              LT -> " (-" <> TB.fromString (show $ before - after) <> ")"
                          timeContent =
                            TB.fromString $
                              formatTime
                                timeLocale
                                "%F %R (JST)"
                                (utcToLocalTime' tzTokyo tLast)
                      in ( " since " <> timeContent
                         , mkTxt stCntPrev stCnt
                         , mkTxt foCntPrev foCnt
                         , mkTxt fodCntPrev fodCnt
                         )
                  contentB :: TB.Builder
                  contentB = mconcat
                    [ "\\[Profile] Twitter stats" <> mSince <> ":\n"
                    , "- Tweets: " <> TB.fromString (show stCnt) <> stDiff <> "\n"
                    , "- Following: " <> TB.fromString (show foCnt) <> foDiff <> "\n"
                    , "- Followers: " <> TB.fromString (show fodCnt) <> fodDiff <> "\n"
                    ]
              Log.i tag "sending new twitter stats"
              writeToTg (TgRMProfileStat (buildStrictText contentB))
              modify (\s -> s {lastStat = Just (curTime, pStat)})
        markEnd
        liftIO $ threadDelay $ 2 * oneSec
