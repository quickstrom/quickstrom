{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Quickstrom.Options where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Quickstrom.Prelude hiding (catch, check, trace)
import Quickstrom.WebDriver.Class
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

defaultCheckOptions :: CheckOptions Maybe
defaultCheckOptions = CheckOptions
  { checkTests = Just 10,
    checkMaxActions = Just $ Size 50,
    checkShrinkLevels = Just 5,
    checkOrigin = Nothing,
    checkMaxTrailingStateChanges = Just 5,
    checkTrailingStateChangeTimeout = Just $ Timeout 50,
    checkWebDriverOptions = Nothing
  }

newtype Size = Size {unSize :: Word32}
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

newtype Timeout = Timeout Word64
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)
