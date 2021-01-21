{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Quickstrom.WebDriver.HsWebDriver where

import Control.Monad (Monad (fail))
import Control.Monad.Catch (MonadCatch (catch), MonadThrow)
import qualified Data.ByteString.Lazy as LBS
import Quickstrom.Element
import Quickstrom.Prelude hiding (catch)
import Quickstrom.Run
import qualified Test.WebDriver as WebDriver

newtype WebDriverClient a = WebDriverClient {unWebDriverClient :: WebDriver.WD a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance WebDriver WebDriverClient where
  getActiveElement = fromRef <$> WebDriverClient WebDriver.activeElem
  isElementEnabled = WebDriverClient . WebDriver.isEnabled . toRef
  getElementTagName = map toS . WebDriverClient . WebDriver.tagName . toRef
  elementClick = WebDriverClient . WebDriver.click . toRef
  elementSendKeys keys = WebDriverClient . WebDriver.sendKeys (toS keys) . toRef
  takeScreenshot = WebDriverClient (LBS.toStrict <$> WebDriver.screenshot)
  findAll (Selector s) = map fromRef <$> WebDriverClient (WebDriver.findElems (WebDriver.ByCSS s))
  navigateTo = WebDriverClient . WebDriver.openPage . toS
  runScript script args = do
    r <- WebDriverClient (WebDriver.asyncJS (map WebDriver.JSArg args) script)
    case r of
      Just (Right a) -> pure a
      Just (Left e) -> liftIO (fail e)
      Nothing -> liftIO (fail "Script timed out")
  catchResponseError ma f =
    ma `catch` \case
      WebDriver.FailedCommand _type msg -> liftIO (f (WebDriverResponseError (toS (WebDriver.errMsg msg))))
  inNewPrivateWindow _logLevel (WebDriverClient action) =
    WebDriverClient $ do
      caps <- WebDriver.getActualCaps
      WebDriver.finallyClose (WebDriver.createSession caps >> action)

runWebDriver :: MonadIO m => WebDriverClient b -> m b
runWebDriver (WebDriverClient ma) =
  liftIO (WebDriver.runSession config ma)

config :: WebDriver.WDConfig
config =
  WebDriver.defaultConfig
    & WebDriver.modifyCaps
      ( \c ->
          c
            { WebDriver.webStorageEnabled = Just False,
              WebDriver.browser =
                WebDriver.Firefox
                  { WebDriver.ffProfile = Nothing,
                    WebDriver.ffLogPref = WebDriver.LogInfo,
                    WebDriver.ffBinary = Nothing,
                    WebDriver.ffAcceptInsecureCerts = Just False
                  },
              WebDriver.proxy = WebDriver.NoProxy
            }
      )
    & WebDriver.useProxy WebDriver.NoProxy

toRef :: Element -> WebDriver.Element
toRef (Element ref) = WebDriver.Element ref

fromRef :: WebDriver.Element -> Element
fromRef (WebDriver.Element ref) = Element ref
