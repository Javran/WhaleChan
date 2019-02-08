{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , ScopedTypeVariables
  , TypeApplications
  #-}
module Javran.WhaleChan.TwitterThread where

import Web.Twitter.Types
import Web.Twitter.Conduit hiding (count)
import Web.Twitter.Conduit.Parameters
import Control.Lens
import qualified Data.ByteString.Char8 as BSC
import Say
import Data.Monoid
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Control.Concurrent
import Control.Monad

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
data TgSyncState
  = TSPending -- indicate that a tweet is detected but not yet sent to the channel
  | TSSynced Int -- indicate that a tweet is already sent as a telegram message
  | TSTimedOut -- tweets acknowledged without syncing to tg (<= tweet-id-greater-than)
  | TSRemoving Int -- indicate that a tweet is removed but channel is not yet notified
  | TSRemoved -- indicate that a tweet is removed and ack-ed with another tg message.
      Int {- first one is for the existing tg msg id -}
      Int {- tg msg id that informs about deletion-}
  | TSDrop -- a TSPending message is not yet synced, so its deleted form has to be dropped
    deriving (Show)

type TweetTracks = M.Map Integer (Status, TgSyncState)

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


oneSec :: Int
oneSec = 1000000

twitterThread :: Manager -> WEnv -> IO ()
twitterThread mgr wenv = do
    let WEnv
          { twWatchingUserId
            {-
              it is intentional that
              this value is ignored for the creation / deletion detection,
              as we really need some "old" data so that the comparing process
              know how to align update list with current state in order to
              detect deletion.

              however, the tg-sync should check twTweetIdGreaterThan
              and turn TSPending to TSTimedOut to prevent flooding the channel
             -}
          -- , twTweetIdGreaterThan
          } = wenv
        twInfo = getTwInfo wenv
        req = userTimeline (UserIdParam (fromIntegral twWatchingUserId))
                & count ?~ 200

    fix (\redo curState -> do
        Response{..} <- callWithResponse twInfo mgr req
        let statusList = responseBody
               -- takeWhile ((> twTweetIdGreaterThan) . statusId) responseBody
            ((tCreated, tDeleted), nextState) = curState `updateTweetStates` statusList
            [rlLimit,rlRemaining,_rlReset] =
              ((read @Int . BSC.unpack) <$>) . (`Prelude.lookup` responseHeaders) <$>
                  [ "x-rate-limit-limit"
                  , "x-rate-limit-remaining"
                  , "x-rate-limit-reset"
                  ]
        case (rlRemaining, rlLimit) of
          (Just rRem, Just rLim)
            | rRem * 5 < rLim ->
              -- rRem / rLim < 20%=1/5 => 5 * rem < lim
              sayString "[tw] warning: rate limit availability < 20%"
          _ -> pure ()
        sayString $ "[tw] created: " <> show (length tCreated) <>
                    ", deleted: " <> show (length tDeleted)
        unless (null tCreated) $
          sayString $ "[tw] created tweets: " <>
            intercalate "," (show . statusId <$> tCreated)
        unless (null tDeleted) $
          sayString $ "[tw] deleted tweets: " <>
            intercalate "," (show . statusId . fst <$> tDeleted)
        threadDelay $ 5 * oneSec
        redo nextState
      ) M.empty

{-
  it might be tempting to use the streaming api, but setting it up is a mess, so, no.

  design assumptions:

  - should tolerate several hours of downtime and we should still be able to catch up
  - however, to avoid flooding the channel, a sensible tweet-id-greater-than must be picked
    prior to execution.

  - more often than not we expect update and cur track to have overlaps,
    this is based on the idea that a single user won't be able
    to add or delete >200 tweets during an interval, which should be fairly safe assumption.

  - note that a tweet record is only removed from state during the phase that we remove stale
    messages (messages that are too old to be part of the comparison), which means
    the latest from the state is the latest id we've seen so far.

 -}

updateTweetStates :: TweetTracks
                  -> [Status]
                  -> (([Status] {- added -}, [(Status, Maybe Int)] {- removed -}), TweetTracks)
updateTweetStates tt upd
  | null upd =
      -- status list empty, should not happen anyway
      (([],[]), tt)
  | null tt =
      -- a new run, we'll need to sync everything.
      let mk s = (statusId s, (s, TSPending))
      in ((upd,[]), M.fromList (mk <$> upd))
  | otherwise =
      let {-
            note that we are using tt rather than tt' here to figure out the correct window
            ttMinId and ttMaxId represents the window in which all tweets could potentially
            be tracked when a new result from API returns.
           -}
          ((ttMinId,_),(ttMaxId,_)) = (M.findMin tt, M.findMax tt)
          updMinId = statusId . last $ upd
          -- NOTE: due to this we cannot detect removal of last tweet
          -- the solution is to have enough tweet up front to "anchor" update list
          -- (this is based on the assumption that user rarely remove old tweets)
          effMinId = max updMinId ttMinId -- effective minId for the window
          -- keep only new ones that could intersect with existing records
          upd1 = takeWhile ((>= effMinId) . statusId) upd
          {-
            now we have:
            - updIntersect, for comparing against tt to determine deleted tweets
            - updNew which consists of all new updates

            note that upd and upd1 are in descending order of status id,
            so upd1 has to be split in this way into updNew and updIntersect
           -}
          (updNew, updIntersect) = span ((> ttMaxId) . statusId) upd1

          stillExist TSRemoving {} = False
          stillExist TSRemoved {} = False
          stillExist TSDrop {} = False
          stillExist _ = True
          -- prepare intersection
          ttIntersect =
            M.filter
              (\(s,ts) ->
                 stillExist ts &&
                 let sId = statusId s in sId >= effMinId && sId <= ttMaxId)
              tt
          -- effectively ttIntersect - updIntersect = ttToDelete
          ttToDelete = appEndo (foldMap (Endo . M.delete . statusId) updIntersect) ttIntersect
          convertDelMark (s,ts) = case ts of
            TSSynced v -> (s, Just v)
            _ -> (s, Nothing)
          -- marking deletion
          tt1 = appEndo (foldMap mark ttToDelete) tt
            where
              mark :: (Status, TgSyncState) -> Endo TweetTracks
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
