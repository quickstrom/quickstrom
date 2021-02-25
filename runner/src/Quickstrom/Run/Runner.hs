{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Quickstrom.Run.Runner where

import Control.Monad (Monad (fail))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import qualified Data.Aeson as JSON
import Data.String (String, fromString)
import Quickstrom.Element (Element)
import Quickstrom.Prelude hiding (catch, check, trace)
import Quickstrom.Specification (Queries)
import Quickstrom.Timeout (Timeout)
import Quickstrom.Trace (ObservedElementStates)
import Quickstrom.WebDriver.Class (WebDriver, WebDriverOptions, runScript)
import Quickstrom.Run.Scripts (CheckScripts)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.URI (URI)

data CheckEnv = CheckEnv {checkOptions :: CheckOptions, checkScripts :: CheckScripts}

newtype Size = Size {unSize :: Word32}
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data CheckOptions = CheckOptions
  { checkTests :: Int,
    checkMaxActions :: Size,
    checkShrinkLevels :: Int,
    checkOrigin :: URI,
    checkMaxTrailingStateChanges :: Int,
    checkTrailingStateChangeTimeout :: Timeout,
    checkWebDriverOptions :: WebDriverOptions,
    checkCaptureScreenshots :: Bool
  }

newtype Runner m a = Runner (ReaderT CheckEnv m a)
  deriving (Functor, Applicative, Monad, MonadIO, WebDriver, MonadReader CheckEnv, MonadThrow, MonadCatch)

run :: CheckEnv -> Runner m a -> m a
run env (Runner ma) = runReaderT ma env
