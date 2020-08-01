{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens hiding (argument)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Symbols.Unicode (bullet)
import Options.Applicative
import qualified Pipes as Pipes
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens (uriScheme)
import qualified Text.URI.QQ as URI
import qualified WebCheck.LogLevel as WebCheck
import WebCheck.Prelude hiding (option)
import qualified WebCheck.Pretty as WebCheck
import qualified WebCheck.PureScript.Program as WebCheck
import qualified WebCheck.Run as WebCheck
import qualified WebCheck.Trace as WebCheck

data WebCheckOptions
  = WebCheckOptions
      { specPath :: FilePath,
        origin :: Text,
        libraryPath :: Maybe FilePath,
        tests :: Int,
        maxActions :: WebCheck.Size,
        shrinkLevels :: Int,
        maxTrailingStateChanges :: Int,
        logLevel :: WebCheck.LogLevel
      }

optParser :: Parser WebCheckOptions
optParser =
  WebCheckOptions
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
    <*> optional
      ( strOption
          ( short 'l'
              <> metavar "DIRECTORY"
              <> long "library-directory"
              <> help "Directory containing compiled PureScript libraries used by WebCheck (falls back to the WEBCHECK_LIBRARY_DIR environment variable)"
          )
      )
    <*> option
      auto
      ( short 'n'
          <> value 10
          <> metavar "NUMBER"
          <> long "tests"
          <> help "How many tests to run"
      )
    <*> ( WebCheck.Size
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
      parseLogLevel
      ( value WebCheck.Info
          <> metavar "LEVEL"
          <> long "log-level"
          <> short 'l'
          <> help "Log level used by WebCheck and the backing WebDriver server (e.g. geckodriver)"
      )
  where
    parseLogLevel = eitherReader $ \case
      "DEBUG" -> pure WebCheck.Debug
      "INFO" -> pure WebCheck.Info
      "WARN" -> pure WebCheck.Warn
      "ERROR" -> pure WebCheck.Error
      s -> Left ("Invalid log level: " <> s)
      

optsInfo :: ParserInfo WebCheckOptions
optsInfo =
  info
    (optParser <**> helper)
    ( fullDesc
        <> header "WebCheck: High-confidence browser testing"
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
  WebCheckOptions {..} <- execParser optsInfo
  originUri <- resolveAbsoluteURI origin
  specResult <- runExceptT $ do
    libPath <- maybe libraryPathFromEnvironment pure libraryPath
    modules <- ExceptT (WebCheck.loadLibraryModules libPath)
    ExceptT (WebCheck.loadSpecificationFile modules specPath)
  case specResult of
    Left err -> do
      hPutStrLn @Text stderr err
      exitWith (ExitFailure 1)
    Right spec -> do
      let opts =
            WebCheck.CheckOptions
              { checkTests = tests,
                checkMaxActions = maxActions,
                checkShrinkLevels = shrinkLevels,
                checkOrigin = originUri,
                checkMaxTrailingStateChanges = maxTrailingStateChanges,
                checkWebDriverLogLevel = logLevel
              }
      result <- Pipes.runEffect (Pipes.for (WebCheck.check opts spec) logEvent)
      logDoc $ line <> divider <> line
      case result of
        WebCheck.CheckFailure {failedAfter, failingTest} -> do
          logDoc $
            WebCheck.prettyTrace (WebCheck.withoutStutterStates (WebCheck.trace failingTest))
          case WebCheck.reason failingTest of
            Just err -> logDoc (line <> annotate (color Red) ("Test failed with error:" <+> err <> line))
            Nothing -> pure ()
          logDoc . annotate (color Red) $
            line <> "Failed after" <+> pretty failedAfter <+> "tests and" <+> pretty (WebCheck.numShrinks failingTest) <+> "levels of shrinking." <> line
          exitWith (ExitFailure 1)
        WebCheck.CheckSuccess ->
          logDoc . annotate (color Green) $
            line <> "Passed" <+> pretty tests <+> "tests." <> line

libraryPathFromEnvironment :: ExceptT Text IO FilePath
libraryPathFromEnvironment = do
  liftIO (lookupEnv (toS key)) >>= \case
    Just p -> pure p
    Nothing -> throwError (key <> "is not set and command-line option is not provided")
  where
    key = "WEBCHECK_LIBRARY_DIR"

logEvent :: WebCheck.CheckEvent -> Pipes.Effect IO ()
logEvent = lift . logDoc . renderCheckEvent

logo :: Doc AnsiStyle
logo =
  -- http://patorjk.com/software/taag/#p=display&f=Ogre&t=WebCheck
  -- with backslashes escaped
  vsep
    [ " __    __     _       ___ _               _    ",
      "/ / /\\ \\ \\___| |__   / __\\ |__   ___  ___| | __",
      "\\ \\/  \\/ / _ \\ '_ \\ / /  | '_ \\ / _ \\/ __| |/ /",
      " \\  /\\  /  __/ |_) / /___| | | |  __/ (__|   < ",
      "  \\/  \\/ \\___|_.__/\\____/|_| |_|\\___|\\___|_|\\_\\"
    ]

divider :: Doc ann
divider = pageWidth $ \(AvailablePerLine w _) -> pretty (Text.replicate w "―")

renderCheckEvent :: WebCheck.CheckEvent -> Doc AnsiStyle
renderCheckEvent = \case
  WebCheck.CheckStarted n ->
    annotate bold logo
      <> line
      <> line
      <> ("Running" <+> annotate (color Blue) (pretty n) <+> "tests...")
  WebCheck.CheckTestEvent e -> renderTestEvent e

renderTestEvent :: WebCheck.TestEvent -> Doc AnsiStyle
renderTestEvent = \case
  WebCheck.TestStarted size ->
    line
      <> divider
      <> line
      <> line
      <> annotate bold (renderSize size <+> "Actions")
  WebCheck.TestPassed size trace' ->
    case traceWarnings size trace' of
      [] -> annotate (color Green) "Test passed!"
      warnings ->
        annotate (color Yellow) "Test passed with warnings:"
          <> line
          <> line
          <> renderList warnings
  WebCheck.TestFailed _size trace' ->
    line <> annotate (color Red) "Test failed:" <> line <> WebCheck.prettyTrace trace'
  WebCheck.Shrinking level ->
    line
      <> annotate bold ("Shrinking failing test down to the" <+> ordinal level <+> "level..." <> line)
  WebCheck.RunningShrink level ->
    "Running shrunk test at level" <+> pretty level <> "."
  where
    traceWarnings :: WebCheck.Size -> WebCheck.Trace WebCheck.TraceElementEffect -> [Doc AnsiStyle]
    traceWarnings size trace' =
      let fewActions =
            case length (trace' ^.. WebCheck.traceActions) of
              0 -> ["Could not generate any valid actions."]
              numActions
                | numActions < fromIntegral (WebCheck.unSize size) ->
                  ["Could only generate" <+> pretty numActions <> "/" <> renderSize size <+> "actions."]
              _ -> []
          actionFailures =
            case trace' ^.. WebCheck.traceActionFailures of
              [] -> []
              failures -> ["There were" <+> pretty (length failures) <+> "action failures"]
       in fewActions <> actionFailures

renderSize :: WebCheck.Size -> Doc AnsiStyle
renderSize (WebCheck.Size s) = pretty s

renderList :: [Doc ann] -> Doc ann
renderList = vsep . map (\x -> bullet <+> align x)

ordinal :: (Pretty n, Integral n) => n -> Doc ann
ordinal n = pretty n <> case n `rem` 10 of
  1 -> "st"
  2 -> "nd"
  3 -> "rd"
  _ -> "th"

logDoc :: Doc AnsiStyle -> IO ()
logDoc = putStrLn . renderStrict . layoutPretty defaultLayoutOptions
