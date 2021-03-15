{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Quickstrom.WebDriver.Class where

import qualified Data.Aeson as JSON
import Quickstrom.Browser
import Quickstrom.Element (Element, Selector)
import Quickstrom.LogLevel
import Quickstrom.Prelude

data WebDriverOptions = WebDriverOptions
  { webDriverBrowser :: Browser,
    webDriverBrowserBinary :: Maybe FilePath,
    webDriverLogLevel :: LogLevel,
    webDriverHost :: Text,
    webDriverPort :: Int,
    webDriverPath :: FilePath,
    webDriverAdditionalOptions :: (Set Text)
  }

class Monad m => WebDriver (m :: Type -> Type) where
  getActiveElement :: m Element
  isElementEnabled :: Element -> m Bool
  getElementTagName :: Element -> m Text
  elementClick :: Element -> m ()
  elementClear :: Element -> m ()
  elementSendKeys :: Text -> Element -> m ()
  takeScreenshot :: m ByteString
  findAll :: Selector -> m [Element]
  navigateTo :: Text -> m ()
  goBack :: m ()
  goForward :: m ()
  pageRefresh :: m ()
  runScript :: JSON.FromJSON r => Text -> [JSON.Value] -> m r
  inNewPrivateWindow :: WebDriverOptions -> m a -> m a

instance WebDriver m => WebDriver (ReaderT e m) where
  getActiveElement = lift getActiveElement
  isElementEnabled = lift . isElementEnabled
  getElementTagName = lift . getElementTagName
  elementClick = lift . elementClick
  elementClear = lift . elementClear
  elementSendKeys keys = lift . elementSendKeys keys
  takeScreenshot = lift takeScreenshot
  findAll = lift . findAll
  navigateTo = lift . navigateTo
  goBack = lift goBack
  goForward = lift goForward
  pageRefresh = lift pageRefresh
  runScript s = lift . runScript s
  inNewPrivateWindow opts (ReaderT ma) = ReaderT (inNewPrivateWindow opts . ma)

data WebDriverResponseError = WebDriverResponseError Text
  deriving (Show, Generic)

instance Exception WebDriverResponseError

data WebDriverOtherError = WebDriverOtherError Text
  deriving (Show, Generic)

instance Exception WebDriverOtherError
