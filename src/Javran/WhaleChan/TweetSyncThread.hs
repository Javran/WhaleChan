{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , ScopedTypeVariables
  , TypeApplications
  , LambdaCase
  , FlexibleContexts
  #-}
module Javran.WhaleChan.TweetSyncThread
  ( tweetSyncThread
  , createTwMVar
  , putTwMsg
  ) where

import Control.Arrow
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Control.Exception
import Data.List
import Web.Twitter.Conduit hiding (count)
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Types
import Data.Time.Clock

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Javran.WhaleChan.Types
import Javran.WhaleChan.Base

import qualified Javran.WhaleChan.Log as Log

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

oneSec :: Int
oneSec = 1000000

createTwMVar :: IO TwMVar
createTwMVar = newMVar Seq.empty

-- https://ghc.haskell.org/trac/ghc/ticket/14810 always takeX then putX ?
putTwMsg :: TwMVar -> TwRxMsg -> IO ()
putTwMsg mv m =
  -- only atomic when there's no other producer
  -- in this case only tg thread talks to tw thread
  modifyMVar_ mv (pure . (Seq.|> m))

tweetSyncThread :: WEnv -> IO ()
tweetSyncThread wenv = do
    t <- getCurrentTime
    {-
      it is intentional that
      "twIgnoreOlderThan"  is ignored for the creation / deletion detection,
      as we really need some "old" data so that the comparing process
      know how to align update list with current state in order to
      detect deletion.
     -}
    let (wconf@WConf{twIgnoreOlderThan, twWatchingUserId}, TCommon{..}) = wenv
        twInfo = getTwInfo wconf
        req = userTimeline (UserIdParam (fromIntegral twWatchingUserId))
                & count ?~ 200
                & tweetMode ?~ "extended"
        startTime = addUTCTime (fromIntegral twIgnoreOlderThan) t
        tweetSyncStep :: TweetSyncM (TweetSyncM ()) -> TweetSyncM ()
        tweetSyncStep markStart = do
            markEnd <- markStart
            mQueue <- liftIO $ swapMVar tcTwitter Seq.empty
            respM <- liftIO $ (Right <$> callWithResponse twInfo tcManager req) `catch`
                    \(e :: SomeException) -> pure (Left e)
            case respM of
              Left e -> do
                Log.e "TweetSync" (displayException e)
                markEnd
                liftIO $ throw e
              _ -> pure ()
            let Right Response{..} = respM
                performUpdate :: TwRxMsg -> TweetTracks -> TweetTracks
                performUpdate (TwRMTgSent tgMsgId twStId) =
                    M.adjust
                      (second $ \case
                          TSPending -> TSSynced tgMsgId
                          TSRemoving v -> TSRemoved v tgMsgId
                          x -> x)
                      twStId
            -- handle received messages
            modify (appDEndo (foldMap (mkDEndo . performUpdate) mQueue))
            let statusList = responseBody
            -- TODO: should have better API to handle gets then modify (const _)
            ((tCreated, tDeleted), nextState) <- gets (`updateTweetStates` statusList)
            modify (const nextState)
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
                      Log.w "TweetSync" "rate limit availability < 20%"
                  _ -> pure ()
            when (length tCreated + length tDeleted > 0) $
              Log.i "TweetSync" $ "created: " <> show (length tCreated) <>
                          ", deleted: " <> show (length tDeleted)
            unless (null tCreated) $ do
              Log.i "TweetSync" $ "created tweets: " <>
                intercalate "," (show . statusId <$> tCreated)
              forM_ tCreated $ \st -> liftIO $  do
                let content = "[tw] " <> statusText st
                -- TODO: set TSTimedOut
                when (statusCreatedAt st > startTime) $
                  writeChan tcTelegram (TgRMTweetCreate (statusId st) content)
            unless (null tDeleted) $ do
              Log.i "TweetSync" $ "deleted tweets: " <>
                intercalate "," (show . statusId . fst <$> tDeleted)
              forM_ tDeleted $ \case
                (st, Just msgId) ->
                  liftIO $ writeChan tcTelegram (TgRMTweetDestroy (statusId st) msgId)
                _ -> pure ()
            markEnd
            liftIO $ threadDelay $ 5 * oneSec
    autoWCM "TweetSync" "tweet-sync.yaml" wenv tweetSyncStep
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
          ttToDelete = appDEndo (foldMap (mkDEndo . M.delete . statusId) updIntersect) ttIntersect
          convertDelMark (s,ts) = case ts of
            TSSynced v -> (s, Just v)
            _ -> (s, Nothing)
          -- marking deletion
          tt1 = appDEndo (foldMap mark ttToDelete) tt
            where
              mark :: (Status, TgSyncState) -> DEndo TweetTracks
              mark (s, ts) = mkDEndo (M.insert k (s,ts'))
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
