{-# LANGUAGE
    OverloadedStrings
  , TypeFamilies
  , RankNTypes
  #-}
module Javran.WhaleChan.ServerStatThread.Base
  ( describeServer
  , getInfoFromKcServer
  , renderVerPackDiffMd
  , renderServerAddrDiffMd
  , parseServerAddr
  ) where

import Data.Aeson
import Data.Char
import Data.List
import Data.Maybe
import Data.Time.Clock
import Network.HTTP.Client
import Text.ParserCombinators.ReadP
import Control.Monad

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB

import Javran.WhaleChan.Util
import Javran.WhaleChan.ServerStatThread.Types

{-
  Basic operations for ServerStatThread
  that does not require knowing type M
 -}

-- known server names
serverNamesTable :: IM.IntMap T.Text
serverNamesTable = IM.fromList
  [ (1 , "横須賀鎮守府")
  , (2 , "呉鎮守府")
  , (3 , "佐世保鎮守府")
  , (4 , "舞鶴鎮守府")
  , (5 , "大湊警備府")
  , (6 , "トラック泊地")
  , (7 , "リンガ泊地")
  , (8 , "ラバウル基地")
  , (9 , "ショートランド泊地")
  , (10 , "ブイン基地")
  , (11 , "タウイタウイ泊地")
  , (12 , "パラオ泊地")
  , (13 , "ブルネイ泊地")
  , (14 , "単冠湾泊地")
  , (15 , "幌筵泊地")
  , (16 , "宿毛湾泊地")
  , (17 , "鹿屋基地")
  , (18 , "岩川基地")
  , (19 , "佐伯湾泊地")
  , (20 , "柱島泊地")
  ]

describeServer :: Int -> T.Text
describeServer sId =
    fromMaybe fallbackName (IM.lookup sId serverNamesTable)
  where
    fallbackName = buildStrictText $
      "KcServer#" <> TB.decimal sId

-- try to download resource from a kcserver
getInfoFromKcServer :: Manager -> String -> IO (VerPack, UTCTime)
getInfoFromKcServer mgr addr = do
  let url = addr <> "kcs2/version.json"
  req <- parseUrlThrow url
  raw <- responseBody <$> httpLbs req mgr
  let Just vp = decode raw
  t <- getCurrentTime
  pure (vp, t)

{-
  message:

  + added: "Added: foo(version), bar(version)"
  + removed: "Removed: foo(version), bar(version)"
  + updated: for this one we'd like to break them into details.

  full message:
  > [ServerStat] Game version changed:
  > + Added: ...
  > + Removed: ...
  > + foo: 0.1.2.3 -> 3.4.5.6
  > + bar: 1.1.1.1 -> 2.2.2.2

 -}
renderVerPackDiffMd :: MapDiffResult M.Map T.Text T.Text -> T.Text
renderVerPackDiffMd ((added, removed), modified) =
    buildStrictText . mconcat . intersperse "\n" $
      "\\[ServerStat] Game Version Updated:"
      : catMaybes [rdrAdded,rdrRemoved] <> fromMaybe [] rdrModified
  where
    simpleRender m
      | xs@(_:_) <- M.toAscList m =
          let render (k,v) = TB.fromText k <> " (" <> TB.fromText v <> ")"
          in Just . mconcat . intersperse ", " . fmap render $ xs
      | otherwise = Nothing
    rdrAdded, rdrRemoved :: Maybe TB.Builder
    rdrModified :: Maybe [TB.Builder]
    rdrAdded = ("- Added: " <>) <$> simpleRender added
    rdrRemoved = ("- Removed: " <>) <$> simpleRender removed
    rdrModified
      | xs@(_:_) <- M.toAscList modified =
          let render (k,(vOld,vNew)) =
                "- " <> TB.fromText k
                <> ": " <> TB.fromText vOld
                <> " -> " <> TB.fromText vNew
          in Just . fmap render $ xs
      | otherwise = Nothing

parseServerAddr :: String -> Maybe String
parseServerAddr raw = case readP_to_S ipP raw of
    [(r,"")] -> Just r
    _ -> Nothing
  where
    ipPartP :: ReadP Int
    ipPartP = do
      xs <- munch1 isDigit
      guard $ length xs < 4
      let r = read xs
      guard $ 0 <= r && r <= 255
      pure r
    ipP = do
      _ <- string "http://"
      a <- ipPartP
      bcd <- replicateM 3 (char '.' *> ipPartP)
      _ <- char '/'
      pure (intercalate "." . map show $ a : bcd)

{-
  we don't expect server address to change too much overtime,
  a slight less structured message (than VerPack) is easy and adequate for this.

  > [ServerStat] Server Address Updated:
  > + Added: <server name>: <ip addr>
  > + Removed: <server name>: <ip addr>
  > + Changed: <server name>: <ip addr> -> <ip addr>

 -}
renderServerAddrDiffMd :: forall m. m ~ IM.IntMap
                       => ((m String, m String), m (String, String)) -> Maybe T.Text
renderServerAddrDiffMd ((added, removed), modified) = do
    let doRender f = fmap f . IM.toAscList
        simpleRender (k,v) = TB.fromText (describeServer k) <> ": " <> TB.fromText (pprIp v)
        renderChange (k,(vOld, vNew)) =
          TB.fromText (describeServer k) <> ": "
          <> TB.fromText (pprIp vOld) <> " -> "
          <> TB.fromText (pprIp vNew)
        renderedLines =
          doRender (("- Added: " <>) . simpleRender) added
          <> doRender (("- Removed: " <>) . simpleRender) removed
          <> doRender (("- Changed: " <>) . renderChange) modified
    case renderedLines of
      [] -> Nothing
      _ ->
        pure . buildStrictText . mconcat . intersperse "\n" $
          "\\[ServerStat] Server Address Updated: "
          : renderedLines
  where
    pprIp :: String -> T.Text
    pprIp raw = T.pack . fromMaybe raw $ parseServerAddr raw
