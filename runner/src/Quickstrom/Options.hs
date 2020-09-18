{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Quickstrom.Options
  ( CheckOptions(..)
  , CheckOptionsFirst
  , CheckOptionsIdentity
  , Size(..)
  , Timeout(..)
  , defaultOptsF
  , selectOption
  )
where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Data.Generic.HKD
import Quickstrom.Prelude hiding (catch, check, trace)
import Quickstrom.WebDriver.Class
-- import Quickstrom.Browser
-- import Quickstrom.LogLevel
import Text.URI

data CheckOptions = CheckOptions
  { checkTests :: Int
  , checkMaxActions :: Size
  , checkShrinkLevels :: Int
  , checkOrigin :: URI
  , checkMaxTrailingStateChanges :: Int
  , checkTrailingStateChangeTimeout :: Timeout
  , checkWebDriverOptions :: WebDriverOptions
  } deriving Generic

type CheckOptionsFirst = HKD CheckOptions First
type CheckOptionsIdentity = HKD CheckOptions Identity

defaultOpts :: CheckOptions
defaultOpts = CheckOptions
  { checkTests = 10
  , checkMaxActions = Size 50
  , checkShrinkLevels = 5
  -- , checkOrigin = Nothing
  , checkMaxTrailingStateChanges = 5
  , checkTrailingStateChangeTimeout = Timeout 50
  -- , checkWebDriverOptions = Nothing
  }

defaultOptsF :: CheckOptionsIdentity
defaultOptsF = deconstruct defaultOpts

-- defaultWebDriverOptions :: WebDriverOptions
-- defaultWebDriverOptions = WebDriverOptions
--   { webDriverBrowser = Firefox,
--     webDriverLogLevel = LogDebug,
--     webDriverPort = 3000
--   }

newtype Size = Size {unSize :: Word32}
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

newtype Timeout = Timeout Word64
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)


selectOption :: Identity a -> [First a] -> a
selectOption optsDefault opts =
  fromMaybe (runIdentity optsDefault) (getFirst $ mconcat opts)
