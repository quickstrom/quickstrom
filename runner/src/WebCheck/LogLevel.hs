module WebCheck.LogLevel where

import WebCheck.Prelude

data LogLevel = LogDebug | LogInfo | LogWarn | LogError
  deriving (Eq, Ord)
