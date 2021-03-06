{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Quickstrom.Run.Runner where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import qualified Data.Aeson as JSON
import Quickstrom.Prelude hiding (catch, check, trace)
import Quickstrom.Run.Scripts (CheckScripts)
import Quickstrom.Timeout (Timeout)
import Quickstrom.WebDriver.Class (WebDriver, WebDriverOptions)
import Text.URI (URI)

data CheckEnv = CheckEnv {checkOptions :: CheckOptions, checkScripts :: CheckScripts}

newtype Size = Size {unSize :: Word32}
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data CheckOptions = CheckOptions
  { checkTests :: Int,
    checkMaxActions :: Size,
    checkMaxShrinks :: Int,
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
