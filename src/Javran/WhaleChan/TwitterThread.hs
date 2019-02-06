{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , TupleSections
  #-}
module Javran.WhaleChan.TwitterThread where

import Web.Twitter.Types
import Web.Twitter.Conduit hiding (count)
import Web.Twitter.Conduit.Parameters
import Control.Lens
import qualified Data.ByteString.Char8 as BSC
import Say
import Data.List
import Data.Monoid
import qualified Data.List.Ordered as LO
import Data.Ord

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
data TweetState
  = TSPending -- indicate that a tweet is detected but not yet sent to the channel
  | TSSynced Int -- indicate that a tweet is already sent as a telegram message
  | TSRemoving Int -- indicate that a tweet is removed but channel is not yet notified
  | TSRemoved -- indicate that a tweet is removed and ack-ed with another tg message.
      Int {- first one is for the existing tg msg id -}
      Int {- tg msg id that informs about deletion-}

data TwState = TwState
  { userIconURLHttps :: Maybe URIString
    -- status id is in descending order to keep it consistent with twitter API
  , tweetStates :: [(Status, TweetState)]
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


  - TODO: should we use IntMap?

 -}

updateTweetStates :: [(Status, TweetState)]
                  -> [Status]
                  -> (([Status] {- added -}, [Status] {- removed -}), [(Status, TweetState)])
updateTweetStates xs [] =
    -- status list empty, should not happen anyway
    (([],[]), xs)
updateTweetStates [] upd =
    -- a new run. we'll need to sync everything
    -- this assumes that upd has gone through tweet-id-greater-than filtering
    -- otherwise we will flood the channel
    ((upd,[]), (,TSPending) <$> upd)
updateTweetStates cur upd = case cur' of
    [] -> ((upd,[]), (,TSPending) <$> upd)
    _ ->
      let curMaxId = statusId . fst . head $ cur'
          curMinId = statusId . fst . last $ cur'
          updMinId = statusId . last $ upd
          cmp = flip (comparing statusId)
          -- remove those old ones (shouldn't be any)
          upd1 = takeWhile ((>= curMinId) . statusId) upd
          {-
            now we have:
            - updIntersect against cur to determine deleted tweets
            - updNew which consists of all new updates
           -}
          (updIntersect, updNew) = span ((<= curMaxId) . statusId) upd1
          updDeleted =
              LO.minusBy cmp
                (takeWhile ((>= updMinId) . statusId) (fst <$> cur'))
                updIntersect
          updatedCur :: [(Status, TweetState)]
          updatedCur =
              -- marking detected deletion as deleted
              appEndo (foldMap markDeleted updNew) cur
            where
              markDeleted :: Status -> Endo [(Status, TweetState)]
              markDeleted s = Endo (f <$>)
                where
                  f p@(st, ts)
                    | statusId st == sId = case ts of
                        TSPending -> (st, TSRemoved 0 0)
                        TSSynced tgId -> (st, TSRemoving tgId)
                        _ -> error "unreachable"
                    | otherwise = p
                  sId = statusId s
      in ((updNew, updDeleted), ((,TSPending) <$> updNew) ++ updatedCur)
  where
    stillExist TSRemoving{} = False
    stillExist TSRemoved{} = False
    stillExist _ = True
    -- respect the fact that some tweets have been removed
    cur' = filter (stillExist . snd) cur
