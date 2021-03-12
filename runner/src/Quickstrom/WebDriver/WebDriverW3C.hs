{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Quickstrom.WebDriver.WebDriverW3C (run) where

import Control.Lens
import Control.Monad.Catch (MonadThrow (..), MonadCatch)
import qualified Control.Monad.Script.Http as ScriptHttp
import qualified Data.Aeson as JSON
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Client as Http
import qualified Network.Wreq as Wreq
import Quickstrom.Browser
import Quickstrom.Element
import Quickstrom.LogLevel
import Quickstrom.Prelude hiding (catch)
import Quickstrom.WebDriver.Class
import Web.Api.WebDriver hiding
  ( Action,
    BrowserName (..),
    LogLevel (..),
    Selector,
    Timeout,
    hPutStrLn,
    runIsolated,
    throwError,
  )
import qualified Web.Api.WebDriver as WebDriver
import Data.Generics.Labels ()
import Data.IORef (IORef)
import qualified Data.IORef as IORef

data WebDriverEnv m = WebDriverEnv { config :: WebDriverConfig m, stateRef :: IORef (S WDState) }
  deriving (Generic)

newtype WebDriverW3C m a = WebDriverW3C (ReaderT (WebDriverEnv IO) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadReader (WebDriverEnv IO))

run :: MonadIO m => WebDriverOptions -> WebDriverW3C m a -> m a
run opts (WebDriverW3C ma) = do
  env <- init opts
  runReaderT ma env

init :: MonadIO m => WebDriverOptions -> m (WebDriverEnv IO)
init WebDriverOptions{..} = do
  mgr <- liftIO (Http.newManager Http.defaultManagerSettings)
  let httpOptions :: Wreq.Options
      httpOptions =
        Wreq.defaults
          & Wreq.manager .~ Right mgr
          & Wreq.headers .~ [("Content-Type", "application/json; charset=utf-8")]
      opts = reconfigure defaultWebDriverConfig httpOptions
  st <- liftIO (IORef.newIORef (_initialState opts))
  pure (WebDriverEnv opts st)
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

runCommand :: (MonadIO m, MonadThrow m) => WebDriverT IO a -> WebDriverW3C m a
runCommand action = do
  WebDriverEnv conf stateRef <- ask
  s <- liftIO (IORef.readIORef stateRef)
  liftIO (execWebDriverT conf { _initialState  = s } action) >>= \case
    (Right x, s', _) -> liftIO (IORef.writeIORef stateRef s') >> pure x
    (Left err, s', _) -> liftIO (IORef.writeIORef stateRef s') >> throwM (WebDriverOtherError (show err))

-- | Mostly the same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
runIsolated ::
  (MonadIO m, MonadThrow m) =>
  WebDriverOptions ->
  WebDriverW3C m a ->
  WebDriverW3C m a
runIsolated opts ma = do
  sid <- runCommand (newSession' (addCaps opts) emptyCapabilities)
  runCommand (modifyState (setSessionId (Just sid)))
  a <- ma
  runCommand deleteSession
  pure a

addCaps :: WebDriverOptions -> JSON.Value -> JSON.Value
addCaps WebDriverOptions {webDriverBrowser = Firefox, webDriverLogLevel, webDriverBrowserBinary} =
  key "capabilities" . key "alwaysMatch" . _Object
    %~ ( <>
           [ ( "moz:firefoxOptions",
               JSON.Object
                 ( [ ("args", JSON.Array ["-headless", "-private"]),
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
                     <> maybe mempty (HashMap.singleton "binary" . JSON.String . toS) webDriverBrowserBinary
                 )
             ),
             ("browserName", "firefox")
           ]
       )
addCaps WebDriverOptions {webDriverBrowser = Chrome, webDriverBrowserBinary, webDriverAdditionalOptions} =
  key "capabilities" . key "alwaysMatch" . _Object
    %~ ( <>
           [ ( "goog:chromeOptions",
               JSON.Object
                 [ ("binary", JSON.String (maybe "/usr/bin/google-chrome" toS webDriverBrowserBinary)),
                   ( "args",
                     JSON.Array
                       ( Vector.fromList
                           ( map
                               JSON.toJSON
                               ( ["headless", "no-sandbox", "disable-gpu", "privileged"]
                                   ++ (["incognito" | not hasUserDataDir])
                                   ++ Set.toList webDriverAdditionalOptions
                               )
                           )
                       )
                   )
                 ]
             ),
             ("browserName", "chrome")
           ]
       )
  where
    hasUserDataDir =
      not
        ( Set.null
            ( Set.filter
                (Text.isPrefixOf "user-data-dir=")
                webDriverAdditionalOptions
            )
        )

-- | Same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
setSessionId ::
  Maybe String.String ->
  S WDState ->
  S WDState
setSessionId x st = st {_userState = (_userState st) {_sessionId = x}}

toRef :: Element -> ElementRef
toRef (Element ref) = ElementRef (Text.unpack ref)

fromRef :: ElementRef -> Element
fromRef (ElementRef ref) = Element (Text.pack ref)

instance (Monad m, MonadIO m, MonadThrow m) => WebDriver (WebDriverW3C m) where
  getActiveElement = fromRef <$> runCommand WebDriver.getActiveElement
  isElementEnabled = runCommand . WebDriver.isElementEnabled . toRef
  getElementTagName = map toS . runCommand . WebDriver.getElementTagName . toRef
  elementClick = runCommand . WebDriver.elementClick . toRef
  elementClear = runCommand . WebDriver.elementClear . toRef
  elementSendKeys keys = runCommand . WebDriver.elementSendKeys (toS keys) . toRef
  takeScreenshot = runCommand WebDriver.takeScreenshot
  findAll (Selector s) = map fromRef <$> runCommand (findElements CssSelector (Text.unpack s))
  navigateTo = runCommand . WebDriver.navigateTo . toS
  goBack = runCommand WebDriver.goBack
  goForward = runCommand WebDriver.goForward
  pageRefresh = runCommand WebDriver.pageRefresh
  runScript script args = do
    r <- runCommand (executeAsyncScript (String.fromString (toS script)) args)
    case JSON.fromJSON r of
      JSON.Success (Right a) -> pure a
      JSON.Success (Left e) -> throwIO (WebDriverOtherError e)
      JSON.Error e -> throwIO (WebDriverOtherError (toS e))
  inNewPrivateWindow = runIsolated