{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , FlexibleContexts
  , ScopedTypeVariables
  , TypeFamilies
  , LambdaCase
  , NoMonomorphismRestriction
  #-}
module Javran.WhaleChan.Util
  ( oneSec
  , oneMin
  , describeDuration
  , isYamlFileNotFoundException
  , eitherToMaybe
  , guardHttpException
  , isConnectionResetException
  , waitUntilStartOfNextMinute
  , dTell
  , checkNetwork
  , buildStrictText
  , displayExceptionShort
  , mapDiff
  , simpleMarkdownEscape
  , flattenJson
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Arrow
import Control.Monad.Writer
import Data.Foldable hiding (find)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Data.Time.Clock
import Data.Aeson
import GHC.IO.Exception
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.Printf

import qualified Data.DList as DL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Types.Status as Tw
import qualified Web.Twitter.Conduit.Response as Tw
import qualified Data.Containers as C
import qualified Data.MonoTraversable as MT
import qualified Data.HashMap.Strict as HM

{-
  place for some commonly used functions.
  functions here should not require Javran.WhaleChan.* to work
 -}

oneSec :: Int
oneSec = 1000000

oneMin :: Int
oneMin = oneSec * 60

describeDuration :: Int -> String
describeDuration seconds
  | seconds == 1 = "1 second"
  | seconds < 60 = show seconds <> " seconds"
  | otherwise =
    let (dd, hh') = seconds `divMod` 86400
        (hh, ss') = hh' `divMod` 3600
        (mm, ss) = ss' `divMod` 60
        ddStr = [ if dd == 1 then "1 day" else show dd <> " days" | dd > 0 ]
        hhStr = [ if hh == 1 then "1 hour" else show hh <> " hours" | hh > 0 ]
        mmStr = [ if mm == 1 then "1 minute" else show mm <> " minutes" | mm > 0 ]
        ssStr = [ if ss == 1 then "1 second" else show ss <> " seconds" | ss > 0 ]
    in unwords $ concat [ddStr, hhStr, mmStr, ssStr]

-- stolen from:
-- https://github.com/snoyberg/yaml/blob/35f0286d83acf6c27e00cf8edbfc43c841109760/yaml/test/Data/Yaml/IncludeSpec.hs#L131-L134
isYamlFileNotFoundException :: Yaml.ParseException -> Bool
isYamlFileNotFoundException (Yaml.InvalidYaml (Just (Yaml.YamlException msg)))
  | "Yaml file not found: " `isPrefixOf` msg = True
isYamlFileNotFoundException _ = False

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

-- for guarding against http-client related exceptions
guardHttpException :: IO a -> IO (Either HttpException a)
guardHttpException action =
  (Right <$> action) `catch` (pure . Left)

isConnectionResetException :: IOException -> Bool
isConnectionResetException ioe
  | IOError
    { ioe_type = ResourceVanished
    , ioe_description = "Connection reset by peer"
    } <- ioe = True
  | otherwise = False

{-
  wait and wake up at (roughly) begining of the next minute
  -- https://stackoverflow.com/a/8578237/315302
 -}
waitUntilStartOfNextMinute :: IO ()
waitUntilStartOfNextMinute = do
    t <- getCurrentTime
    -- compute millseconds since beginning of current minute
    let ms = round (fromIntegral oneSec * realToFrac @_ @Double (utctDayTime t)) `rem` oneMin
    -- wait to start of next minute
    threadDelay $ oneMin - ms

dTell :: MonadWriter (DL.DList a) m => a -> m ()
dTell = tell . DL.singleton

-- | check network connection
checkNetwork :: IO Bool
checkNetwork =
    catch checkGoogle (\(_ :: SomeException) -> pure False)
  where
    checkGoogle =  do
      -- one shot manager. in case there are caching related behaviors
      mgr <- newManager tlsManagerSettings
      req <- parseUrlThrow "http://www.google.com/generate_204"
      resp <- httpNoBody req mgr
      let hdrs = responseHeaders resp
      pure $! hdrs `deepseq` True

buildStrictText :: TB.Builder -> T.Text
buildStrictText = TL.toStrict . TB.toLazyText

{-
  like displayException but will try to shorten the description of
  known and insignificant exceptions
  so they don't take too much space in logs
 -}
displayExceptionShort :: SomeException -> String
displayExceptionShort se
  | Just (HttpExceptionRequest _ ConnectionTimeout) <- fromException se
    = "HttpConnectionTimeout"
  | Just (HttpExceptionRequest _ ResponseTimeout) <- fromException se
    = "HttpResponseTimeout"
  | Just (HttpExceptionRequest _ (InternalException e)) <- fromException se
    = "HttpResponseInternalException: " <> displayException e
  | Just (
      Tw.TwitterErrorResponse
      Tw.Status {Tw.statusCode = stCode}
      _
      [ Tw.TwitterErrorMessage
          { Tw.twitterErrorCode = twErrCode
          , Tw.twitterErrorMessage = twErrMsg
          }
      ]
    ) <- fromException se
    = printf "TwitterError: st=%d, err=%d, msg=%s" stCode twErrCode twErrMsg
  | otherwise
    = displayException se



mapDiff :: forall ks m mp k v .
           {-
             type variables explained:
             - m, k, v: the map type m that we are diff-ing,
                 whose key is k and value v.
             - mp: same as m, except the value is of type (v,v)
                 to indicate that the value is changed
                 from old (fst) to new one (snd)
             - ks: type for return value of keysSet
            -}
           ( C.HasKeysSet m
           , C.IsMap m
           , k ~ C.ContainerKey m
           , v ~ C.MapValue m
           , C.IsMap mp
           , k ~ C.ContainerKey mp
           , (v,v) ~ C.MapValue mp
           , Eq v
           , C.SetContainer ks
           , C.KeySet m ~ ks
           , MT.Element ks ~ k
           )
        => m -> m
        -> ((m, m), mp)
mapDiff mBefore mAfter = ((added, removed), modified)
  where
    added = mAfter `C.difference` mBefore
    removed = mBefore `C.difference` mAfter
    ksBefore = C.keysSet mBefore
    ksAfter = C.keysSet mAfter
    -- the set of keys being preseved (existing before and after)
    ksPreserved = ksBefore `C.intersection` ksAfter
    find :: k -> mp
    find k =
      if valBefore == valAfter
        then mempty
        else C.singletonMap k (valBefore, valAfter)
      where
        valBefore = fromJust (C.lookup k mBefore)
        valAfter = fromJust (C.lookup k mAfter)
    modified = MT.ofoldMap find ksPreserved

{-
  ref:
  - https://stackoverflow.com/a/49924429/315302
  - https://core.telegram.org/bots/api#markdown-style
 -}
simpleMarkdownEscape :: T.Text -> T.Text
simpleMarkdownEscape =
    T.replace "_" "\\_"
    . T.replace "*" "\\*"
    . T.replace "[" "\\["
    . T.replace "`" "\\`"

{-
  Turn a JSON Object into a flat Text-to-Text map
  - to flatten already-flat value is straightforward.
  - to flatten an Object, we do it in a way that nested values are flatten
    to top-level
  - to flatten an Array, we first recursively flatten all its values
    then wrap it in "[]", separate by ","
 -}
flattenJson :: Object -> HM.HashMap T.Text T.Text
flattenJson = HM.fromList . DL.toList . foldMap (uncurry flattenKeyVal) . HM.toList
  where
    tShow = T.pack . show
    wrapList xs = "[" <> T.intercalate "," xs <> "]"

    flattenValue :: Value -> T.Text
    flattenValue = \case
      Object m -> flattenObject m
      Array xs -> wrapList $ toList (flattenValue <$> xs)
      String t -> t
      Number n -> tShow n
      Bool b -> tShow b
      Null -> "Null"

    flattenObject :: Object -> T.Text
    flattenObject m =
        wrapList $ pprKv <$>
          toList (foldMap (uncurry flattenKeyVal) (HM.toList m))
      where
        pprKv (k,v) = "(" <> tShow k <> "," <> v <> ")"

    flattenKeyVal :: T.Text -> Value -> DL.DList (T.Text, T.Text)
    flattenKeyVal k = \case
        Object m ->
          first ((k <>).("." <>)) <$> foldMap (uncurry flattenKeyVal) (HM.toList m)
        x -> DL.singleton (k, flattenValue x)
