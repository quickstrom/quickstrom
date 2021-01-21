{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens hiding (argument)
import Control.Monad.Catch (try)
import Data.Generics.Labels ()
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Symbols.Unicode (bullet)
import Options.Applicative
import qualified Pipes as Pipes
import qualified Quickstrom.Browser as Quickstrom
import qualified Quickstrom.CLI.Logging as Quickstrom
import Quickstrom.CLI.Reporter (Reporter)
import qualified Quickstrom.CLI.Reporter as Reporter
import qualified Quickstrom.CLI.Reporter.Console as Reporter
import qualified Quickstrom.CLI.Reporter.HTML as Reporter
import qualified Quickstrom.LogLevel as Quickstrom
import Quickstrom.Prelude hiding (option, try)
import qualified Quickstrom.PureScript.Program as Quickstrom
import qualified Quickstrom.Run as Quickstrom
import qualified Quickstrom.Trace as Quickstrom
import qualified Quickstrom.WebDriver.Class as Quickstrom
import qualified Quickstrom.WebDriver.WebDriverW3C as WebDriver
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    hSetEncoding,
    utf8,
  )
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens (uriScheme)
import qualified Text.URI.QQ as URI

data CLI = CLI
  { chosenCommand :: Command,
    libraryPath :: Maybe FilePath,
    logLevel :: Quickstrom.LogLevel
  }

data Command = Check CheckOptions | Lint LintOptions

data CheckOptions = CheckOptions
  { specPath :: FilePath,
    origin :: Text,
    tests :: Int,
    maxActions :: Quickstrom.Size,
    shrinkLevels :: Int,
    maxTrailingStateChanges :: Int,
    trailingStateChangeTimeout :: Word64,
    captureScreenshots :: Bool,
    browser :: Quickstrom.Browser,
    browserOptions :: Set Text,
    browserBinary :: Maybe FilePath,
    webDriverHost :: Text,
    webDriverPort :: Int,
    webDriverPath :: FilePath,
    reporters :: [Text],
    htmlReportDirectory :: FilePath
  }

data LintOptions = LintOptions
  { specPath :: FilePath
  }

checkOptionsParser :: Parser CheckOptions
checkOptionsParser =
  CheckOptions
    <$> argument
      str
      ( metavar "SPECIFICATION_FILE"
          <> help "A specification file to check"
      )
    <*> argument
      str
      ( metavar "URI"
          <> help "The origin URI"
      )
    <*> option
      auto
      ( short 'n'
          <> value 10
          <> metavar "NUMBER"
          <> long "tests"
          <> help "How many tests to run"
      )
    <*> ( Quickstrom.Size
            <$> option
              auto
              ( value 100
                  <> metavar "NUMBER"
                  <> long "max-actions"
                  <> help "Maximum number of actions to generate in the largest test"
              )
        )
    <*> option
      auto
      ( short 's'
          <> value 10
          <> metavar "NUMBER"
          <> long "shrink-levels"
          <> help "How many levels to shrink the generated actions after a failed test"
      )
    <*> option
      auto
      ( value 0
          <> metavar "NUMBER"
          <> long "max-trailing-state-changes"
          <> help "Maximum number of trailing state changes to await"
      )
    <*> option
      auto
      ( value 100
          <> metavar "NUMBER"
          <> long "trailing-state-change-timeout"
          <> help "The initial timeout for awaited state changes (doubles with each await)"
      )
    <*> switch
      ( long "capture-screenshots"
          <> help "Capture a screenshot at each state, and record the positions of queried elements"
      )
    <*> option
      (eitherReader Quickstrom.parseBrowser)
      ( metavar "BROWSER"
          <> long "browser"
          <> short 'b'
          <> value Quickstrom.Firefox
          <> help "Browser used (through WebDriver) to run tests"
      )
    <*> option
      (maybeReader Quickstrom.parseBrowserOptions)
      ( metavar "BROWSER_OPTIONS"
          <> long "browser-options"
          <> value Set.empty
          <> help "Additional options to pass directly to the browser"
      )
    <*> optional
      ( option
          str
          ( metavar "PATH"
              <> long "browser-binary"
              <> help "The absolute path to the binary of the web browser used for testing"
          )
      )
    <*> option
      str
      ( metavar "HOST"
          <> value "127.0.0.1"
          <> long "webdriver-host"
          <> help "The host (DNS name or IP) of the WebDriver HTTP server"
      )
    <*> option
      auto
      ( metavar "PORT"
          <> value 4444
          <> long "webdriver-port"
          <> help "The port of the WebDriver HTTP server"
      )
    <*> option
      str
      ( metavar "PATH"
          <> value ""
          <> long "webdriver-path"
          <> help "The relative path of the WebDriver root HTTP resource"
      )
    <*> many
      ( option
          (eitherReader parseRunnerName)
          ( metavar "NAME"
              <> long "reporter"
              <> help "Name of one or more reporters to use (defaults to only \"console\")"
          )
      )
    <*> option
      str
      ( metavar "DIR"
          <> long "html-report-directory"
          <> value "html-report"
          <> help "Output directory of generated HTML report"
      )

lintOptionsParser :: Parser LintOptions
lintOptionsParser =
  LintOptions
    <$> argument
      str
      ( metavar "SPECIFICATION_FILE"
          <> help "A specification file to check"
      )

cliParser :: Parser CLI
cliParser =
  CLI
    <$> cmds
      <*> optional
        ( strOption
            ( metavar "DIRECTORY"
                <> long "library-directory"
                <> help "Directory containing compiled PureScript libraries used by Quickstrom (falls back to the QUICKSTROM_LIBRARY_DIR environment variable)"
            )
        )
    <*> option
      (eitherReader Quickstrom.parseLogLevel)
      ( value Quickstrom.LogInfo
          <> metavar "LEVEL"
          <> long "log-level"
          <> short 'l'
          <> help "Log level used by Quickstrom and the backing WebDriver server (e.g. geckodriver)"
      )
  where
    cmds =
      subparser
        ( command "check" (info (Check <$> checkOptionsParser) (progDesc "Check a web application using a specification"))
            <> command "lint" (info (Lint <$> lintOptionsParser) (progDesc "Lint a specification"))
        )

optsInfo :: ParserInfo CLI
optsInfo =
  info
    (cliParser <**> helper)
    ( fullDesc
        <> header "Quickstrom: High-confidence browser testing"
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  CLI {..} <- execParser optsInfo
  libPath <- maybe libraryPathFromEnvironment pure libraryPath
  case chosenCommand of
    Check cOpts@CheckOptions {..} -> do
      originUri <- resolveAbsoluteURI origin
      specResult <- runExceptT $ do
        modules <- ExceptT (Quickstrom.loadLibraryModules libPath)
        ExceptT (Quickstrom.loadSpecificationFile modules specPath)
      case specResult of
        Left err -> do
          hPutStrLn @Text stderr err
          exitWith (ExitFailure 2)
        Right spec -> flip runReaderT logLevel $ do
          let wdOpts =
                Quickstrom.WebDriverOptions
                  { webDriverLogLevel = logLevel,
                    webDriverBrowser = browser,
                    webDriverBrowserBinary = browserBinary,
                    webDriverAdditionalOptions = browserOptions,
                    webDriverHost,
                    webDriverPort,
                    webDriverPath
                  }
              opts =
                Quickstrom.CheckOptions
                  { checkTests = tests,
                    checkMaxActions = maxActions,
                    checkShrinkLevels = shrinkLevels,
                    checkOrigin = originUri,
                    checkMaxTrailingStateChanges = maxTrailingStateChanges,
                    checkTrailingStateChangeTimeout = Quickstrom.Timeout trailingStateChangeTimeout,
                    checkWebDriverOptions = wdOpts,
                    checkCaptureScreenshots = captureScreenshots
                  }

          let chosenReporterNames = if null reporters then ["console"] else reporters
              chosenReporters = filter ((`elem` chosenReporterNames) . fst) (initializeReporters cOpts)
          reporterPreCheckErrors <-
            concatMapM
              ( \(name, r) ->
                  Reporter.preCheck r wdOpts opts >>= \case
                    Reporter.OK -> pure []
                    Reporter.CannotBeInvoked reason -> pure [(name, reason)]
              )
              chosenReporters
          if not (null reporterPreCheckErrors)
            then do
              Quickstrom.logDoc . Quickstrom.logSingle (Just Quickstrom.LogError) . annotate (color Red) $
                "The following reporters cannot be invoked:" <> line <> renderList [pretty name <> ":" <+> reason | (name, reason) <- reporterPreCheckErrors]
            else do
              result <-
                Pipes.runEffect (Pipes.for (Quickstrom.check opts (WebDriver.runWebDriver wdOpts) spec) (lift . Quickstrom.logDoc . renderCheckEvent))
                  & try
              Quickstrom.logDoc . Quickstrom.logSingle Nothing $ line <> divider <> line
              case result of
                Right checkResult ->
                  for_ chosenReporters $ \(_, r) ->
                    Reporter.report r wdOpts opts checkResult
                Left err@SomeException {} -> do
                  Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Red) $
                    line <> "Check encountered an error:" <+> pretty (show err :: Text) <> line
              exitWithResult result
    Lint LintOptions {..} -> do
      specResult <- runExceptT $ do
        modules <- ExceptT (Quickstrom.loadLibraryModules libPath)
        ExceptT (Quickstrom.loadSpecificationFile modules specPath)
      flip runReaderT logLevel $ case specResult of
        Left err -> do
          Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Red) $ "Specification did not pass linting."
          hPutStrLn @Text stderr err
          liftIO (exitWith (ExitFailure 1))
        Right _spec ->
          Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Green) $ "Specification passed linting."

availableReporters :: (MonadIO m, MonadReader Quickstrom.LogLevel m) => [(Text, CheckOptions -> Reporter m)]
availableReporters =
  [ ("console", const Reporter.consoleReporter),
    ("html", \opts -> Reporter.htmlReporter (htmlReportDirectory opts))
  ]

reporterNames :: [Text]
reporterNames = map fst (availableReporters @(ReaderT Quickstrom.LogLevel IO))

initializeReporters :: (MonadIO m, MonadReader Quickstrom.LogLevel m) => CheckOptions -> [(Text, Reporter m)]
initializeReporters opts = [(name, mk opts) | (name, mk) <- availableReporters]

parseRunnerName :: [Char] -> Either [Char] Text
parseRunnerName s =
  let t = toS s
   in if t `elem` reporterNames then Right t else Left ("Invalid reporter: " <> s)

exitWithResult :: MonadIO m => Either SomeException Quickstrom.CheckResult -> m ()
exitWithResult = \case
  Right Quickstrom.CheckSuccess {} ->
    pass
  Right Quickstrom.CheckFailure {} -> do
    liftIO (exitWith (ExitFailure 3))
  Right Quickstrom.CheckError {} -> do
    liftIO (exitWith (ExitFailure 1))
  Left {} ->
    liftIO (exitWith (ExitFailure 1))

libraryPathFromEnvironment :: IO FilePath
libraryPathFromEnvironment = do
  lookupEnv key >>= \case
    Just p -> pure p
    Nothing -> do
      hPutStrLn stderr (key <> "is not set and command-line option is not provided")
      exitWith (ExitFailure 1)
  where
    key = "QUICKSTROM_LIBRARY_DIR"

logo :: Doc AnsiStyle
logo =
  -- http://patorjk.com/software/taag/#p=display&f=Ogre&t=Quickstrom
  -- with backslashes escaped
  vsep
    [ "   ____       _      _        _                               ",
      "  /___ \\_   _(_) ___| | _____| |_ _ __ ___  _ __ ___         ",
      " //  / / | | | |/ __| |/ / __| __| '__/ _ \\| '_ ` _ \\       ",
      "/ \\_/ /| |_| | | (__|   <\\__ \\ |_| | | (_) | | | | | |     ",
      "\\___,_\\ \\__,_|_|\\___|_|\\_\\___/\\__|_|  \\___/|_| |_| |_|"
    ]

divider :: Doc ann
divider = pageWidth $ \(AvailablePerLine w _) -> pretty (Text.replicate w "―")

renderCheckEvent :: Quickstrom.CheckEvent -> [(Maybe Quickstrom.LogLevel, Doc AnsiStyle)]
renderCheckEvent = \case
  Quickstrom.CheckStarted n ->
    Quickstrom.logSingle Nothing $
      annotate bold logo
        <> line
        <> line
        <> ("Running" <+> annotate (color Blue) (pretty n) <+> "tests...")
  Quickstrom.CheckTestEvent e -> renderTestEvent e
  Quickstrom.CheckFinished {} -> mempty

renderTestEvent :: Quickstrom.TestEvent -> [(Maybe Quickstrom.LogLevel, Doc AnsiStyle)]
renderTestEvent = \case
  Quickstrom.TestStarted size ->
    Quickstrom.logSingle Nothing $
      line
        <> divider
        <> line
        <> line
        <> annotate bold (renderSize size <+> "actions")
  Quickstrom.TestPassed size trace' ->
    case traceWarnings size trace' of
      [] ->
        Quickstrom.logSingle Nothing $ annotate (color Green) "Test passed!"
      warnings ->
        [ (Nothing, annotate (color Green) "Test passed!"),
          ( Just Quickstrom.LogWarn,
            line
              <> annotate (color Yellow) "Warnings:"
              <> line
              <> line
              <> renderList warnings
          )
        ]
  Quickstrom.TestFailed _size _trace ->
    Quickstrom.logSingle Nothing $
      line <> annotate (color Red) "Test failed"
  Quickstrom.Shrinking level ->
    Quickstrom.logSingle Nothing $
      line
        <> annotate bold ("Shrinking failing test down to the" <+> ordinal level <+> "level..." <> line)
  Quickstrom.RunningShrink level ->
    Quickstrom.logSingle (Just Quickstrom.LogInfo) $
      "Running shrunk test at level" <+> pretty level <> "."
  where
    traceWarnings :: Quickstrom.Size -> Quickstrom.Trace Quickstrom.TraceElementEffect -> [Doc AnsiStyle]
    traceWarnings size trace' =
      let fewActions =
            case length (trace' ^.. Quickstrom.traceActions) of
              0 -> ["Could not generate any valid actions."]
              numActions
                | numActions < fromIntegral (Quickstrom.unSize size) ->
                  ["Could only generate" <+> pretty numActions <> "/" <> renderSize size <+> "actions."]
              _ -> []
          actionFailures =
            case trace' ^.. Quickstrom.traceActionFailures of
              [] -> []
              failures -> ["There were" <+> pretty (length failures) <+> "action failures"]
       in fewActions <> actionFailures

renderSize :: Quickstrom.Size -> Doc AnsiStyle
renderSize (Quickstrom.Size s) = pretty s

renderList :: [Doc ann] -> Doc ann
renderList = vsep . map (\x -> bullet <+> align x)

ordinal :: (Pretty n, Integral n) => n -> Doc ann
ordinal 11 = "11th"
ordinal 12 = "12th"
ordinal n =
  pretty n <> case n `rem` 10 of
    1 -> "st"
    2 -> "nd"
    3 -> "rd"
    _ -> "th"
