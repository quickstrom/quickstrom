{-# LANGUAGE LambdaCase #-}

-- | Browsers supported by Quickstrom.
module Quickstrom.Browser where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (String)
import Data.Text (pack, words)
import Quickstrom.Prelude

data Browser = Firefox | Chrome

parseBrowser :: String -> Either String Browser
parseBrowser = \case
  "firefox" -> pure Firefox
  "chrome" -> pure Chrome
  s -> Left ("Invalid or unsupported browser: " <> s)

parseBrowserOptions :: String -> Maybe (Set Text)
parseBrowserOptions s =
  let set = Set.fromList $ words $ pack s
   in if (Set.null set) then Nothing else Just set
