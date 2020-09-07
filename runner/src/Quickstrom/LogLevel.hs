{-# LANGUAGE LambdaCase #-}

module Quickstrom.LogLevel where

import Data.String (String)
import Quickstrom.Prelude

data LogLevel = LogDebug | LogInfo | LogWarn | LogError
  deriving (Eq, Ord)

parseLogLevel :: String -> Either String LogLevel
parseLogLevel = \case
  "DEBUG" -> pure LogDebug
  "INFO" -> pure LogInfo
  "WARN" -> pure LogWarn
  "ERROR" -> pure LogError
  s -> Left ("Invalid log level: " <> s)
