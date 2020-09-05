{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import Control.Lens hiding (argument)
import qualified Data.Aeson as JSON
import qualified Data.Binary.Builder as Builder
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Lazy as TL
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Middleware, pathInfo, responseLBS)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
import qualified Options.Applicative as Options
import qualified Pipes as Pipes
import qualified Quickstrom.LogLevel as Quickstrom
import Quickstrom.Prelude hiding (get, option)
import qualified Quickstrom.PureScript.Program as Quickstrom
import qualified Quickstrom.Run as Quickstrom
import qualified Quickstrom.Browser as Quickstrom
import qualified Quickstrom.WebDriver.Class as WebDriver
import qualified Quickstrom.WebDriver.WebDriverW3C as WebDriver
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens (uriScheme)
import qualified Text.URI.QQ as URI
import Web.Scotty.Trans

newtype CheckId = CheckId {unCheckId :: Text}
  deriving (Eq, Show, Generic, Hashable)

data Env = Env {modules :: Quickstrom.Modules, webOptions :: WebOptions, scheduledChecks :: MVar (HashMap CheckId ScheduledCheck)}

type App = ScottyT TL.Text (ReaderT Env IO) ()

data ScheduledCheck = ScheduledCheck
  { scheduledCheckEventsIn :: Chan.InChan Quickstrom.CheckEvent,
    scheduledCheckEventsOut :: Chan.OutChan Quickstrom.CheckEvent,
    scheduledCheckResult :: Maybe (Either Text Quickstrom.CheckResult)
  }

data SpecForm = SpecForm {code :: Text, origin :: Text}
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data ErrorEntity = ErrorEntity {error :: Text}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data ScheduledCheckState = Running | Finished
  deriving (Show, Generic, JSON.ToJSON)

data CheckScheduledEntity = CheckScheduledEntity {state :: ScheduledCheckState, uri :: Text, message :: Text}
  deriving (Show, Generic, JSON.ToJSON)

data ScheduledCheckEntity = ScheduledCheckEntity {state :: ScheduledCheckState, checkResult :: Maybe Quickstrom.CheckResult}
  deriving (Show, Generic, JSON.ToJSON)

app :: WebOptions -> Env -> App
app WebOptions {..} Env {..} = do
  middleware logStdoutDev
  middleware (sendCheckEvents scheduledChecks)
  middleware (staticPolicy (addBase staticFilesPath))
  get "/" (file (staticFilesPath </> "index.html"))
  let modifyChecks f = liftIO (modifyMVar scheduledChecks (pure . (,()) . f))
      modifyCheck checkId f = modifyChecks (HashMap.adjust f checkId)
  post "/checks" do
    form <- jsonData
    originUri <- liftAndCatchIO (resolveAbsoluteURI (origin form))
    specResult <- liftAndCatchIO (Quickstrom.loadSpecification modules (code form))
    case specResult of
      Left err -> status HTTP.status400 >> json err
      Right spec -> do
        checkId <- CheckId . UUID.toText <$> liftIO UUID.nextRandom
        (eventsIn, eventsOut) <- liftIO (Chan.newChan 1000)
        modifyChecks (HashMap.insert checkId (ScheduledCheck eventsIn eventsOut Nothing))
        let wdOpts =
              WebDriver.WebDriverOptions
                { webDriverBrowser = Quickstrom.Firefox,
                  webDriverBrowserBinary = Nothing,
                  webDriverLogLevel = logLevel,
                  webDriverHost = "localhost",
                  webDriverPort = 4444,
                  webDriverPath = mempty
                }
            opts =
              Quickstrom.CheckOptions
                { checkTests = tests,
                  checkShrinkLevels = shrinkLevels,
                  checkOrigin = originUri,
                  checkMaxActions = maxActions,
                  checkMaxTrailingStateChanges = maxTrailingStateChanges,
                  checkWebDriverOptions = wdOpts
                }
        let action = do
              result <-
                Pipes.runEffect
                  ( Pipes.for (Quickstrom.check opts (WebDriver.runWebDriver wdOpts) spec) \event ->
                      liftIO (Chan.writeChan eventsIn event)
                  )
              modifyCheck checkId (\c -> c {scheduledCheckResult = Just (Right result)})
        liftAndCatchIO . void . forkIO $
          action `catch` \(SomeException e) ->
            modifyCheck checkId (\c -> c {scheduledCheckResult = Just (Left (show e))})
        let checkUri = baseUri <> "/checks/" <> unCheckId checkId
        json (CheckScheduledEntity Running checkUri "Check scheduled")
  get "/checks/:checkId" do
    checkId <- CheckId <$> param "checkId"
    checks <- liftIO (readMVar scheduledChecks)
    case HashMap.lookup checkId checks of
      Just check' ->
        case scheduledCheckResult check' of
          Nothing -> (status HTTP.status202 >> json (ScheduledCheckEntity Running Nothing))
          Just (Left err) -> json (ErrorEntity err)
          Just (Right res) -> json (ScheduledCheckEntity Finished (Just res))
      Nothing -> status HTTP.status404 >> json (ErrorEntity "Check not found")

sendCheckEvents :: MVar (HashMap CheckId ScheduledCheck) -> Middleware
sendCheckEvents var app' req respond =
  case pathInfo req of
    ["checks", CheckId -> checkId, "events"] -> do
      checks <- readMVar var
      case HashMap.lookup checkId checks of
        Just ScheduledCheck {..} -> do
          let nextEvent = do
                ev <- Chan.readChan scheduledCheckEventsOut
                pure (ServerEvent (Just "") Nothing [Builder.fromLazyByteString (JSON.encode ev)])
           in eventSourceAppIO nextEvent req respond
        Nothing -> respond (responseLBS HTTP.status404 [] "Check not found")
    _ -> app' req respond

main :: IO ()
main = do
  webOptions <- Options.execParser optsInfo
  modulesResult <- runExceptT do
    libPath <- maybe libraryPathFromEnvironment pure (libraryPath webOptions)
    ExceptT (Quickstrom.loadLibraryModules libPath)
  case modulesResult of
    Left err -> do
      hPutStrLn @Text stderr err
      exitWith (ExitFailure 1)
    Right modules -> do
      scheduledChecks <- newMVar mempty
      let env = Env {modules, webOptions, scheduledChecks}
      scottyT 8080 (flip runReaderT env) (app webOptions env)

renderString :: Doc () -> TL.Text
renderString = toS . renderStrict . layoutPretty defaultLayoutOptions

-- * Options

data WebOptions = WebOptions
  { libraryPath :: Maybe FilePath,
    staticFilesPath :: FilePath,
    tests :: Int,
    shrinkLevels :: Int,
    maxActions :: Quickstrom.Size,
    maxTrailingStateChanges :: Int,
    logLevel :: Quickstrom.LogLevel,
    baseUri :: Text
  }

optParser :: Options.Parser WebOptions
optParser =
  WebOptions
    <$> optional
      ( Options.strOption
          ( Options.long "library-directory"
              <> Options.help "Directory containing compiled PureScript libraries used by Quickstrom (falls back to the QUICKSTROM_LIBRARY_DIR environment variable)"
          )
      )
    <*> Options.strOption
      ( Options.short 's'
          <> Options.long "static-files-directory"
          <> Options.help "Directory containing static files"
          <> Options.value "static"
      )
    <*> Options.option
      Options.auto
      ( Options.short 'n'
          <> Options.value 10
          <> Options.long "tests"
          <> Options.help "How many tests to run"
      )
    <*> Options.option
      Options.auto
      ( Options.short 's'
          <> Options.value 10
          <> Options.long "shrink-levels"
          <> Options.help "How many levels to shrink the generated actions after a failed test"
      )
    <*> ( Quickstrom.Size
            <$> Options.option
              Options.auto
              ( Options.value 100
                  <> Options.metavar "NUMBER"
                  <> Options.long "max-actions"
                  <> Options.help "Maximum number of actions to generate in the largest test"
              )
        )
    <*> Options.option
      Options.auto
      ( Options.value 0
          <> Options.metavar "NUMBER"
          <> Options.long "max-trailing-state-changes"
          <> Options.help "Maximum number of trailing state changes to await"
      )
    <*> Options.option
      (Options.eitherReader Quickstrom.parseLogLevel)
      ( Options.value Quickstrom.LogInfo
          <> Options.metavar "LEVEL"
          <> Options.long "log-level"
          <> Options.short 'l'
          <> Options.help "Log level used by Quickstrom and the backing WebDriver server (e.g. geckodriver)"
      )
    <*> Options.option
      Options.str
      ( Options.metavar "URI"
          <> Options.long "base-uri"
          <> Options.help "Base URI for links"
      )

optsInfo :: Options.ParserInfo WebOptions
optsInfo =
  Options.info
    (optParser <**> Options.helper)
    ( Options.fullDesc
        <> Options.header "Quickstrom: High-confidence browser testing"
    )

fileScheme :: URI.RText 'URI.Scheme
fileScheme = [URI.scheme|file|]

resolveAbsoluteURI :: Text -> IO URI
resolveAbsoluteURI t = do
  uri <- URI.mkURI t
  case uri ^. uriScheme of
    Just scheme | scheme /= fileScheme -> pure uri
    _ ->
      URI.makeAbsolute fileScheme <$> (URI.mkURI . toS =<< canonicalizePath (toS t))

libraryPathFromEnvironment :: ExceptT Text IO FilePath
libraryPathFromEnvironment = do
  liftIO (lookupEnv (toS key)) >>= \case
    Just p -> pure p
    Nothing -> throwError (key <> "is not set and command-line option is not provided")
  where
    key = "QUICKSTROM_LIBRARY_DIR"
