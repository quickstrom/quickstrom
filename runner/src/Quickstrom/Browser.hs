{-# LANGUAGE LambdaCase #-}

-- | Browsers supported by Quickstrom.
module Quickstrom.Browser where

import Data.String (String)
import Data.Set (Set)
import Data.String (words)
import qualified Data.Set as Set
import Quickstrom.Prelude

data Browser = Firefox | Chrome

parseBrowser :: String -> Either String Browser
parseBrowser = \case
  "firefox" -> pure Firefox
  "chrome" -> pure Chrome
  s -> Left ("Invalid or unsupported browser: " <> s)

parseBrowserOptions :: String -> Maybe (Set String)
parseBrowserOptions  s =
  let set = Set.fromList $ words s
      in if (Set.null set) then Nothing else Just set
