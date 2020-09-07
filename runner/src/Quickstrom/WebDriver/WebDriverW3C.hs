{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Quickstrom.WebDriver.WebDriverW3C where

import Control.Lens
import Control.Monad (Monad (fail))
import Control.Monad.Catch (MonadThrow (..))
import qualified Control.Monad.Script.Http as ScriptHttp
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Identity (IdentityT (..))
import qualified Data.Aeson as JSON
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.String (String)
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http
import qualified Network.Wreq as Wreq
import Quickstrom.Browser
import Quickstrom.Element
import Quickstrom.LogLevel
import Quickstrom.Prelude hiding (catch)
import Quickstrom.WebDriver.Class
import Web.Api.WebDriver hiding (Action, BrowserName (..), LogLevel (..), Selector, Timeout, hPutStrLn, runIsolated, throwError)
import qualified Web.Api.WebDriver as WebDriver

newtype WebDriverW3C m a = WebDriverW3C {unWebDriverW3C :: WebDriverTT IdentityT m a}
  deriving (Functor, Applicative, Monad)

instance MonadTrans WebDriverW3C where
  lift = WebDriverW3C . liftWebDriverTT . IdentityT

instance MonadIO m => MonadIO (WebDriverW3C m) where
  liftIO = WebDriverW3C . liftWebDriverTT . liftIO

instance MonadIO m => MonadThrow (WebDriverW3C m) where
  throwM = liftIO . throwM

instance MonadIO m => WebDriver (WebDriverW3C m) where
  getActiveElement = fromRef <$> WebDriverW3C WebDriver.getActiveElement
  isElementEnabled = WebDriverW3C . WebDriver.isElementEnabled . toRef
  getElementTagName = map toS . WebDriverW3C . WebDriver.getElementTagName . toRef
  elementClick = WebDriverW3C . WebDriver.elementClick . toRef
  elementSendKeys keys = WebDriverW3C . WebDriver.elementSendKeys (toS keys) . toRef
  findAll (Selector s) = map fromRef <$> WebDriverW3C (findElements CssSelector (Text.unpack s))
  navigateTo = WebDriverW3C . WebDriver.navigateTo . toS
  runScript script args = do
    r <- WebDriverW3C (executeAsyncScript (String.fromString (toS script)) args)
    case JSON.fromJSON r of
      JSON.Success (Right a) -> pure a
      JSON.Success (Left e) -> throwIO (WebDriverOtherError e)
      JSON.Error e -> throwIO (WebDriverOtherError (toS e))

  catchResponseError (WebDriverW3C ma) f =
    WebDriverW3C $
      ma `WebDriver.catchError` \case
        ResponseError _ msg _ _ _ -> liftWebDriverTT (liftIO (f (WebDriverResponseError (toS msg))))
        err -> liftWebDriverTT (throwIO (WebDriverOtherError (show err)))

  inNewPrivateWindow = runIsolated

runWebDriver :: MonadIO m => WebDriverOptions -> WebDriverW3C m b -> m b
runWebDriver WebDriverOptions {..} (WebDriverW3C ma) = do
  mgr <- liftIO (Http.newManager Http.defaultManagerSettings)
  let httpOptions :: Wreq.Options
      httpOptions =
        Wreq.defaults
          & Wreq.manager .~ Right mgr
          & Wreq.headers .~ [("Content-Type", "application/json; charset=utf-8")]
  execWebDriverT (reconfigure defaultWebDriverConfig httpOptions) ma >>= \case
    (Right x, _, _) -> pure x
    (Left err, _, _) -> liftIO (fail (show err))
  where
    reconfigure c httpOptions =
      c
        { _environment =
            (_environment c)
              { _logEntryPrinter = logPrinter,
                _env =
                  defaultWDEnv
                    { _remoteHostname = toS webDriverHost,
                      _remotePort = webDriverPort,
                      _remotePath = webDriverPath
                    }
              },
          _initialState = defaultWebDriverState {_httpOptions = httpOptions},
          _evaluator = liftIO . _evaluator c
        }
    logPrinter opts logEntry =
      let minSeverity = case webDriverLogLevel of
            LogDebug -> ScriptHttp.LogDebug
            LogInfo -> ScriptHttp.LogInfo
            LogWarn -> ScriptHttp.LogWarning
            LogError -> ScriptHttp.LogError
       in WebDriver.basicLogEntryPrinter opts {WebDriver._logMinSeverity = minSeverity} logEntry

-- | Mostly the same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
runIsolated ::
  Monad m =>
  WebDriverOptions ->
  WebDriverW3C m a ->
  WebDriverW3C m a
runIsolated opts (WebDriverW3C theSession) = WebDriverW3C . cleanupOnError $ do
  sid <- newSession' (addCaps opts) emptyCapabilities
  modifyState (setSessionId (Just sid))
  a <- theSession
  deleteSession
  modifyState (setSessionId Nothing)
  pure a

addCaps :: WebDriverOptions -> JSON.Value -> JSON.Value
addCaps WebDriverOptions {webDriverBrowser = Firefox, webDriverLogLevel, webDriverBrowserBinary} =
  key "capabilities" . key "alwaysMatch" . _Object
    %~ ( <>
           [ ( "moz:firefoxOptions",
               JSON.Object
                 [ ("binary", JSON.String (maybe "/home/vlad/.nix-profile/bin/firefox" toS webDriverBrowserBinary)),
                   ("args", JSON.Array ["-headless", "-private"]),
                   ("prefs", JSON.Object [("Dom.storage.enabled", JSON.Bool False)]),
                   ( "log",
                     JSON.Object
                       [ ( "level",
                           case webDriverLogLevel of
                             LogDebug -> "debug"
                             LogInfo -> "info"
                             LogWarn -> "warn"
                             LogError -> "error"
                         )
                       ]
                   )
                 ]
             ),
             ("browserName", "firefox")
           ]
       )
addCaps WebDriverOptions {webDriverBrowser = Chrome, webDriverBrowserBinary} =
  key "capabilities" . key "alwaysMatch" . _Object
    %~ ( <>
           [ ( "goog:chromeOptions",
               JSON.Object
                 [ ("binary", JSON.String (maybe "/usr/bin/google-chrome" toS webDriverBrowserBinary)),
                   ("args", JSON.Array ["headless", "incognito", "no-sandbox", "disable-gpu", "privileged"])
                 ]
             ),
             ("browserName", "chrome")
           ]
       )

-- | Same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
cleanupOnError ::
  (Monad eff, Monad (t eff), MonadTrans t) =>
  -- | `WebDriver` session that may throw errors
  WebDriverTT t eff a ->
  WebDriverTT t eff a
cleanupOnError x =
  catchAnyError
    x
    (\e -> deleteSession >> WebDriver.throwError e)
    (\e -> deleteSession >> throwHttpException e)
    (\e -> deleteSession >> throwIOException e)
    (\e -> deleteSession >> throwJsonError e)

-- | Same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
setSessionId ::
  Maybe String ->
  S WDState ->
  S WDState
setSessionId x st = st {_userState = (_userState st) {_sessionId = x}}

toRef :: Element -> ElementRef
toRef (Element ref) = ElementRef (Text.unpack ref)

fromRef :: ElementRef -> Element
fromRef (ElementRef ref) = Element (Text.pack ref)
