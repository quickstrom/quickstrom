{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Quickstrom.Options where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Quickstrom.Prelude hiding (catch, check, trace)
import Quickstrom.WebDriver.Class
import Quickstrom.Browser
import Quickstrom.LogLevel
import Text.URI

data CheckOptions f = CheckOptions
  { checkTests :: f Int,
    checkMaxActions :: f Size,
    checkShrinkLevels :: f Int,
    checkOrigin :: f URI,
    checkMaxTrailingStateChanges :: f Int,
    checkTrailingStateChangeTimeout :: f Timeout,
    checkWebDriverOptions :: f WebDriverOptions
    }

defaultCheckOptions :: CheckOptions Identity
defaultCheckOptions = CheckOptions
  { checkTests = 10,
    checkMaxActions = Identity $ Size 50,
    checkShrinkLevels = 5,
    checkMaxTrailingStateChanges = 5,
    checkTrailingStateChangeTimeout = Identity $ Timeout 50,
    checkWebDriverOptions = Identity defaultWebDriverOptions
  }

defaultWebDriverOptions :: WebDriverOptions
defaultWebDriverOptions = WebDriverOptions
  { webDriverBrowser = Firefox,
    webDriverLogLevel = LogDebug,
    webDriverPort = 3000
  }

newtype Size = Size {unSize :: Word32}
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

newtype Timeout = Timeout Word64
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)
