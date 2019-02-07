{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  #-}
module Javran.WhaleChan.TwitterThread where

import Web.Twitter.Types
import Web.Twitter.Conduit hiding (count)
import Web.Twitter.Conduit.Parameters
import Control.Lens
import qualified Data.ByteString.Char8 as BSC
import Say
import Data.Monoid
import qualified Data.Map.Strict as M

import Javran.WhaleChan.Types

{-
  design draft:

  - note that rate limit applies on a per-user per-application basis
    which means we have 900 calls for a 15 min window in total

  - for each 10 seconds, retrieve without since_id and with count=200

    + we'll be using 6 per min * 15 = 90 calls, way below the limit for a decent response time.
    + the intention of not using since_id is to retrieve tweet ids
      of the past so that deletion can be detected
    + talk to telegramThread when there's a need of sending messages

  - also for each 10 seconds, get account icon & user following count
    (TODO: this part is not planned in implementation yet)

  - the whole process will be:

    + get tweets and notify about changes (if any)
    + sleep for 5 seconds
    + get account icon & other info, notify about changes (if any)
    + sleep for 5 seconds
    + <loop>

  data structure:

  - see below

  - all detected tweet or deleted tweet will be sent to telegram thread exactly once,
    we'll notify about failure in log (and will never retry for simplicity)

  - the sequence will have a lower bound and upper bound of length limit,
    when upon hitting the upper bound, old items are removed from front until
    reaching lower bound. or in other words, we don't want to keep track of too many messages
    and neither do we want to do cleanup too often.

  - initial limit might be just lower bound = 5, upper bound = 10 to see if it works correctly.
  - tentative limit: lower bound = 512, upper bound = 1024 after correctness can be confirmed.

  starting monitoring:

  - we'll need to start monitoring, but we don't want to
    flood the channel instantly with all messages we got from twitter,
    in order to do that, we'll set a minimum number of tweet id,
    and anything we get lower than that, we'll ignore.
 -}

-- for keeping track of sync-state between twitter thread and telegram thread
data TweetSyncState
  = TSPending -- indicate that a tweet is detected but not yet sent to the channel
  | TSSynced Int -- indicate that a tweet is already sent as a telegram message
  | TSRemoving Int -- indicate that a tweet is removed but channel is not yet notified
  | TSRemoved -- indicate that a tweet is removed and ack-ed with another tg message.
      Int {- first one is for the existing tg msg id -}
      Int {- tg msg id that informs about deletion-}
  | TSDrop -- a TSPending message is not yet synced, so its deleted form has to be dropped

type TweetTracks = M.Map Integer (Status, TweetSyncState)

data TwState = TwState
  { userIconURLHttps :: Maybe URIString
    -- status id is in descending order to keep it consistent with twitter API
  , tweetTracks :: TweetTracks
  }

getTwInfo :: WEnv -> TWInfo
getTwInfo WEnv{..} = TWInfo twTok Nothing
  where
    oauth = twitterOAuth
              { oauthConsumerKey = twConsumerKey
              , oauthConsumerSecret = twConsumerSecret
              }
    credential = Credential
                 [ ("oauth_token", BSC.pack twOAuthToken)
                 , ("oauth_token_secret", BSC.pack twOAuthSecret)
                 ]
    twTok = TWToken
              oauth
              credential

-- TODO: at some point in time we should peek into header just to make sure
-- rate-limit is okay
twitterThread :: Manager -> WEnv -> IO ()
twitterThread mgr wenv = do
    let WEnv
          { twWatchingUserId
          , twTweetIdGreaterThan
          } = wenv
        twInfo = getTwInfo wenv
        req = userTimeline (UserIdParam (fromIntegral twWatchingUserId))
                & count ?~ 200
    statusList <- takeWhile ((> twTweetIdGreaterThan) . statusId) <$> call twInfo mgr req
    sayString $ "[twitter] printing status ids: " <> show (statusId <$> statusList)
    pure ()

{-
  it might be tempting to use the streaming api, but setting it up is a mess, so, no.

  design assumptions:

  - should tolerate several hours of downtime and we should still be able to catch up
  - however, to avoid flooding the channel, a sensible tweet-id-greater-than must be picked
    prior to execution.

  - more often than not we expect update and cur track to have overlaps,
    this is based on the idea that a single user won't be able
    to add or delete >200 tweets during a interval, which should be fairly safe assumption.

  - TODO: should we use IntMap?

 -}

updateTweetStates :: TweetTracks
                  -> [Status]
                  -> (([Status] {- added -}, [(Status, Maybe Int)] {- removed -}), TweetTracks)
updateTweetStates tt upd
  | null upd =
      -- status list empty, should not happen anyway
      (([],[]), tt)
  | null tt' =
      -- a new run, or in an almost impossible case
      -- that all recorded tweets are deleted during the interval.
      -- in short we'll need to sync everything.
      -- this assumes that upd has gone through tweet-id-greater-than filtering
      -- otherwise we will flood the channel
      let mk s = (statusId s, (s, TSPending))
      in ((upd,[]), M.fromList (mk <$> upd))
  | otherwise =
      -- note that we are using tt rather than tt' here to figure out the correct window
      let ((ttMinId,_),(ttMaxId,_)) = (M.findMin tt, M.findMax tt)
          updMinId = statusId . last $ upd
          effMinId = max updMinId ttMinId -- effective minId for the window
          -- keep only new ones that could intersect with existing records
          upd1 = takeWhile ((>= effMinId) . statusId) upd
          {-
            now we have:
            - updIntersect, for comparing against tt to determine deleted tweets
            - updNew which consists of all new updates
           -}
          (updIntersect, updNew) = span ((<=ttMaxId).statusId) upd1
          -- prepare intersect by zooming into the window
          ttIntersect =
            M.filter
              (\(s,ts) ->
                 stillExist ts &&
                 let sId = statusId s in sId >= effMinId && sId <= ttMaxId)
              tt
          ttToDelete = appEndo (foldMap (Endo . M.delete . statusId) updIntersect) ttIntersect
          convertDelMark (s,ts) = case ts of
            TSSynced v -> (s, Just v)
            _ -> (s, Nothing)
          -- marking deletion
          tt1 = appEndo (foldMap mark ttToDelete) tt
            where
              mark :: (Status, TweetSyncState) -> Endo TweetTracks
              mark (s, ts) = Endo (M.insert k (s,ts'))
                where
                  k = statusId s
                  ts' = case ts of
                    TSPending -> TSDrop
                    TSSynced v -> TSRemoving v
                    _ -> error "unreachable" -- because of "stillExist" predicate.
          -- inserting new upd
          mk s = (statusId s, (s, TSPending))
          tt2 = tt1 <> M.fromList (mk <$> updNew)
      in ((updNew, convertDelMark <$> M.elems ttToDelete),tt2)
  where
    stillExist TSRemoving{} = False
    stillExist TSRemoved{} = False
    stillExist TSDrop{} = False
    stillExist _ = True
    -- respect the fact that some tweets have been removed
    tt' = M.filter (stillExist . snd) tt
