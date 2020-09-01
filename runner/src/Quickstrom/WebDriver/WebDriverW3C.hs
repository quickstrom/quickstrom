{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Quickstrom.WebDriver.WebDriverW3C where

import Control.Lens
import Control.Monad (Monad (fail))
import Control.Monad.Catch (MonadThrow (..))
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
import Quickstrom.Element
import Quickstrom.LogLevel
import Quickstrom.Prelude hiding (catch)
import Quickstrom.Run
import Web.Api.WebDriver hiding (Action, LogLevel (..), Selector, Timeout, hPutStrLn, runIsolated, throwError)
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

  inNewPrivateWindow logLevel =
    runIsolated (defaultChromeCapabilities)
    where
      reconfigure c =
        c
          { -- _firefoxOptions =
            -- (_firefoxOptions c)
            -- <&> \o ->
            -- o
            -- { _firefoxArgs = Just ["-headless", "-private"],
            -- _firefoxPrefs = Just (HashMap.singleton "Dom.storage.enabled" (JSON.Bool False)),
            -- _firefoxLog = Just (FirefoxLog (Just (webdriverLogLevel logLevel)))
            -- },
            _chromeOptions =
              Just
                defaultChromeOptions
                  { _chromeBinary = Just "google-chrome-stable"
                  --,  _chromeArgs = Just ["--no-sandbox", "--no-dev-shm-usage"]
                  }
          }
      webdriverLogLevel = \case
        LogDebug -> WebDriver.LogDebug
        LogInfo -> WebDriver.LogInfo
        LogWarn -> WebDriver.LogWarn
        LogError -> WebDriver.LogError

runWebDriver :: MonadIO m => WebDriverW3C m b -> m b
runWebDriver (WebDriverW3C ma) = do
  mgr <- liftIO (Http.newManager Http.defaultManagerSettings)
  let httpOptions :: Wreq.Options
      httpOptions = Wreq.defaults & Wreq.manager .~ Right mgr
  execWebDriverT (reconfigure defaultWebDriverConfig httpOptions) ma >>= \case
    (Right x, _, _) -> pure x
    (Left err, _, _) -> liftIO (fail (show err))
  where
    reconfigure c httpOptions =
      c
        { _environment =
            (_environment c)
              { _logEntryPrinter = \_ _ -> Nothing,
                _env = defaultWDEnv {_remoteHostname = "127.0.0.1"}
              },
          _initialState = defaultWebDriverState {_httpOptions = httpOptions},
          _evaluator = liftIO . _evaluator c
        }

-- | Mostly the same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
runIsolated ::
  Monad m =>
  Capabilities ->
  WebDriverW3C m a ->
  WebDriverW3C m a
runIsolated caps (WebDriverW3C theSession) = WebDriverW3C . cleanupOnError $ do
  sid <- newSession' addCaps caps
  modifyState (setSessionId (Just sid))
  a <- theSession
  deleteSession
  modifyState (setSessionId Nothing)
  pure a
  where
    addCaps :: JSON.Value -> JSON.Value
    addCaps =
      key "capabilities" . key "alwaysMatch" . _Object
        %~ ( <>
               [ ( "goog:chromeOptions",
                   JSON.Object
                     [ ("binary", "/nix/store/wv5f8h1m3wjjcgd20sf13x10bwyvf4ms-google-chrome-84.0.4147.105/bin/google-chrome-stable")
                     , ("args", JSON.Array ["headless", "incognito"])
                     ]
                 )
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
