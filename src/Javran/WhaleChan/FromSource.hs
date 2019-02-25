{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , DefaultSignatures
  , TypeFamilies
  , DataKinds
  #-}
module Javran.WhaleChan.FromSource where

import Data.Time.Clock
import Web.Twitter.Conduit (Manager)

import Javran.WhaleChan.Types
import qualified Javran.WhaleChan.FromSource.KcsConst as KcsConst
import qualified Javran.WhaleChan.FromSource.Kc3Kai as Kc3Kai
import qualified Javran.WhaleChan.FromSource.Wikia as Wikia
import qualified Javran.WhaleChan.FromSource.Kcwiki as Kcwiki

-- typeclass for external sources (e.g. Wikia, Kc3Kai)
class FromSource src where
  type ExtData src

  -- for fetching & parsing info from raw sources
  fetchInfo :: p src -> Manager -> IO (Maybe (ExtData src))
  -- for getting maintenance time from parsed external data
  toMaintenanceTime :: p src -> ExtData src -> Maybe (PRange UTCTime)

  default toMaintenanceTime ::
    (PRange UTCTime ~ ExtData src) => p src -> ExtData src -> Maybe (PRange UTCTime)
  toMaintenanceTime _ = pure

{-
  this module is for figuring out next maintenance time from various sources

  possible sources are:

  - game source: http://203.104.209.7/gadget_html5/js/kcs_const.js
  - Kc3Kai: https://raw.githubusercontent.com/KC3Kai/KC3Kai/master/update
  - Kcwiki: https://zh.kcwiki.org/wiki/Template:维护倒数
  - Wikia: https://kancolle.fandom.com/wiki/Recent_Updates

  notes:

  - Date.parse spec as impl in v8: https://github.com/v8/v8/blob/8bb236d7c91cc1cbc5ddb656b57bcaa51eeb5b54/src/dateparser-inl.h#L24-L70
    too complicated to impl for our case.

  - what we do instead is to create just the right date format for time package to parse
    certainly this is not the idea solution, but given that all these sources are
    maintained and kept sane by human, I would't expect the format to be change much,
    plus we'll immediate notice if there are changes that breaks the format.

 -}

sourceTest :: WEnv -> IO ()
sourceTest e = do
    let (_,TCommon{tcManager = mgr}) = e
    putStrLn "KcsConst"
    putStr "  " >> KcsConst.getInfo mgr >>= \(Just (KcsConst.KcsConst _ t)) -> print t
    putStrLn "Kc3Kai"
    putStr "  " >> Kc3Kai.getInfo mgr >>= print
    putStrLn "Wikia"
    putStr "  " >> Wikia.getInfo mgr >>= print
    putStrLn "Kcwiki"
    putStr "  " >> Kcwiki.getInfo mgr >>= print
