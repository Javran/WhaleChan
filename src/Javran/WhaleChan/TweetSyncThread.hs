{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , ScopedTypeVariables
  , LambdaCase
  , FlexibleContexts
  , OverloadedLabels
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
import Data.List
import Data.Time.Clock
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Status
import Web.Twitter.Types

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB

import Javran.WhaleChan.Types
import Javran.WhaleChan.Util
import Javran.WhaleChan.Base
import Javran.WhaleChan.Twitter
import Javran.WhaleChan.HealthThread (heartbeat)

import qualified Javran.WhaleChan.Log as Log

{-

  - note that rate limit applies on a per-user, per-application, per-api basis
    which means we have 900 calls for a 15 min window in total for just the
    purpose of syncing tweets, which is plenty.

  - for each 3 seconds, retrieve without since_id and with count=200

    + the intention of not using since_id is to retrieve tweet ids
      of the past so that deletion can be detected
    + talk to telegramThread when there's a need of sending messages

  data structure:

  - see below

  - all detected tweet or deleted tweet will be sent to telegram thread exactly once,
    we'll notify about failure in log (and will never retry for simplicity)

 -}

createTwMVar :: IO TwMVar
createTwMVar = newMVar Seq.empty

{-
  it seems that as long as we always do "takeX-then-putX",
  we should be deadlock-free.

  ref: https://ghc.haskell.org/trac/ghc/ticket/14810 always takeX then putX
 -}
putTwMsg :: TwMVar -> TwRxMsg -> IO ()
putTwMsg mv m =
  modifyMVar_ mv (pure . (Seq.|> m))

{-
  because:
  (1) telegram tries to be smart and preview all links
  (2) we want user to be able to go to the original tweet,
      which by default will be previewed, so the actual content show up twice
      (second time as preview)

  preview should not be always disabled.
  we'll still need preview to be present in the case a tweet contains
  either media or url.

 -}
shouldPreview :: Status -> Bool
shouldPreview st
  | Just TweetUrls
    { otherUrls = eus
    , mediaUrls = ems
    } <- statusGetTweetUrls st
    = not (null eus && null ems)
  | otherwise = False

{-
  For tweets with medias, We'd like to prepend the link to media before showing
  the content of the tweet, this makes sure that media gets previewed.
 -}
mediaPrependMarkdown :: Status -> Maybe T.Text
mediaPrependMarkdown st = do
  TweetUrls {mediaUrls = ems@(_:_)} <- statusGetTweetUrls st
  let mediaToMdLink num mUrl = "[" <> TB.decimal num <> "](" <> TB.fromText mUrl <> ")"
      mediaMdLinks = zipWith mediaToMdLink [1::Int ..] ems
      mediaContentMd = "Media: " <> mconcat (intersperse ", " mediaMdLinks)
  -- Media: [1](<link>), [2](<link>), ...
  pure . buildStrictText $ mediaContentMd

tweetSyncThread :: WEnv -> IO ()
tweetSyncThread wenv = do
    t <- getCurrentTime
    {-
      it is intentional that
      "twIgnoreOlderThan" is ignored for the creation / deletion detection,
      as we really need some "old" data so that the comparing process
      know how to align update list with current state in order to
      detect deletion.
     -}
    let (WConf{twIgnoreOlderThan, twWatchingUserId}, TCommon{..}) = wenv
        {-
          TODO: note that in the following request,
          we intentionally includes entities, which would be useful
          if in future we want to format tweets with markdown.
          (in order to make embeded links / images work better)
         -}
        req = userTimeline (UserIdParam (fromIntegral twWatchingUserId))
                & #count ?~ 200
                & #tweet_mode ?~ "extended"
        -- we ignore all messages older than a specific duration right
        -- after the thread is started, by doing so, we make sure not to flood
        -- the channel with old tweets (even if they are not yet sync-ed)
        startTime = addUTCTime (-fromIntegral twIgnoreOlderThan) t
        loggerIO = wenvToLoggerIO wenv
        tag = "TweetSync"
        hb = heartbeat tag 3600 -- kill this thread if it doesn't come back in 1 hr
    Log.i' loggerIO tag $
      "Will ignore tweets created before " <> show startTime
    let tweetSyncStep :: TweetSyncM (TweetSyncM ()) -> TweetSyncM ()
        tweetSyncStep markStart = do
            markEnd <- markStart
            hb
            mQueue <- liftIO $ swapMVar tcTwitter Seq.empty
            let info = liftIO . Log.i' loggerIO tag
            callTwApi tag req $ \statusList -> do
                {-
                  handle received messages.

                  for now only TelegramThread sends message to this thread
                  in order to establish pairs between tweet status id and
                  telegram message id.
                 -}
                let performUpdate :: TwRxMsg -> TweetTracks -> TweetTracks
                    performUpdate (TwRMTgSent tgMsgId twStId) =
                      M.adjust
                        (second $ \case
                            TSPending -> TSSynced tgMsgId
                            TSRemoving v -> TSRemoved v tgMsgId
                            x -> x)
                        twStId
                modify (appDEndo (foldMap (mkDEndo . performUpdate) mQueue))
                {-
                  after incoming message is handled, we now look at result
                  of api call `statusList`, and recognize created and deleted tweets.
                 -}
                (tCreated, tDeleted) <- state (`updateTweetStates` statusList)
                unless (null tCreated) $ do
                  info $ "created tweets: " <>
                    intercalate "," (show . statusId <$> tCreated)
                  {-
                    as tCreated is in descending order of time, we'll need to consider
                    every tweet in backward order to keep tg channel's history in sync
                   -}
                  forM_ (reverse tCreated) $ \st -> do
                    -- TODO: clean this up
                    let mdLink = createTweetLinkMarkdown tzTokyo st
                        content = case mediaPrependMarkdown st of
                          Nothing ->
                            let escContent =
                                  simpleMarkdownEscape $ "[Tweet] " <> statusText st
                            in escContent <> "\n" <> mdLink
                          Just mp ->
                            simpleMarkdownEscape "[Tweet] " <> mp <> "\n"
                            <> simpleMarkdownEscape (statusText st) <> "\n"
                            <> mdLink
                    if statusCreatedAt st > startTime
                      then do
                        info $ "push status " <> show (statusId st) <> " to tg"
                        writeToTg $
                          TgRMTweetCreate
                            (statusId st)
                            content
                            (shouldPreview st)
                      else do
                        let stId = statusId st
                        info $ "status " <> show stId <> " ignored (outdated)"
                        modify $ M.insert stId (st, TSIgnored)
                -- TODO: we need to double check deleted tweets before handing over the message
                -- to telegram, this is because twitter api could return stale data,
                -- so a sudden disappearance of tweet does not necessarily mean that tweet is actually deleted.
                unless (null tDeleted) $ do
                  info $ "deleted tweets: " <>
                    intercalate "," (show . statusId . fst <$> tDeleted)
                  forM_ tDeleted $ \case
                    (st, Just msgId) ->
                      writeToTg $ TgRMTweetDestroy (statusId st) msgId
                    _ -> pure ()
            markEnd
            liftIO $ threadDelay $ 3 * oneSec
    autoWCM tag "tweet-sync.yaml" wenv tweetSyncStep
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

updateTweetStates
  :: TweetTracks
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
