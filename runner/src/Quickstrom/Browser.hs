{-# LANGUAGE LambdaCase #-}

-- | Browsers supported by Quickstrom.
module Quickstrom.Browser where

import Data.String (String)
import Quickstrom.Prelude

data Browser = Firefox | Chrome

parseBrowser :: String -> Either String Browser
parseBrowser = \case
  "firefox" -> pure Firefox
  "chrome" -> pure Chrome
  s -> Left ("Invalid or unsupported browser: " <> s)
