{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module WebCheck.Run.WebDriverW3C where

import Control.Lens
import Control.Monad (Monad (fail))
import Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.String (String)
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http
import qualified Network.Wreq as Wreq
import Protolude
import Web.Api.WebDriver hiding (Action, LogLevel (..), Selector, Timeout, hPutStrLn, runIsolated, throwError)
import qualified Web.Api.WebDriver as WebDriver
import WebCheck.Element
import WebCheck.LogLevel
import WebCheck.Run
import Control.Monad.Trans.Identity (IdentityT(..))
import Protolude.Error (error)

newtype WebDriverW3C m a = WebDriverW3C { unWebDriverW3C :: WebDriverTT IdentityT m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans WebDriverW3C where
    lift = WebDriverW3C . liftWebDriverTT . IdentityT

instance MonadIO m => MonadIO (WebDriverW3C m) where
  liftIO = WebDriverW3C . liftWebDriverTT . liftIO

instance MonadError WebDriverError m => MonadError WebDriverError (WebDriverW3C m) where
  catchError (WebDriverW3C ma) f = WebDriverW3C $ ma `WebDriver.catchError` \case
    ResponseError _ msg _ _ _ -> unWebDriverW3C (f (WebDriverError (toS msg)))
    err -> unWebDriverW3C (f (WebDriverError (show err)))
  throwError err = lift (throwError err)

instance MonadReader e m => MonadReader e (WebDriverW3C m) where
  ask = lift ask
  local = error "Can't use local"
    

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
      JSON.Success (Left e) -> liftIO (fail e)
      JSON.Error e -> liftIO (fail e)
  inNewPrivateWindow CheckOptions {checkWebDriverLogLevel} =
    runIsolated (reconfigure headlessFirefoxCapabilities)
    where
      reconfigure c =
        c
          { _firefoxOptions =
              (_firefoxOptions c)
                <&> \o ->
                  o
                    { _firefoxArgs = Just ["-headless", "-private"],
                      _firefoxPrefs = Just (HashMap.singleton "Dom.storage.enabled" (JSON.Bool False)),
                      _firefoxLog = Just (FirefoxLog (Just (webdriverLogLevel checkWebDriverLogLevel)))
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
              { _logEntryPrinter = \_ _ -> Nothing
              },
          _initialState = defaultWebDriverState {_httpOptions = httpOptions}
          , _evaluator = liftIO . _evaluator c
        }

-- | Mostly the same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
runIsolated ::
  Monad m =>
  Capabilities ->
  WebDriverW3C m a ->
  WebDriverW3C m a
runIsolated caps (WebDriverW3C theSession) = WebDriverW3C . cleanupOnError $ do
  sid <- newSession caps
  modifyState (setSessionId (Just sid))
  a <- theSession
  deleteSession
  modifyState (setSessionId Nothing)
  pure a

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
