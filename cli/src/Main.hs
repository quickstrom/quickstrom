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
import Control.Monad.Catch (try)
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
import qualified Quickstrom.LogLevel as Quickstrom
import Quickstrom.Prelude hiding (option, try)
import qualified Quickstrom.Pretty as Quickstrom
import qualified Quickstrom.PureScript.Program as Quickstrom
import qualified Quickstrom.Run as Quickstrom
import qualified Quickstrom.WebDriver.WebDriverW3C as WebDriver
import qualified Quickstrom.Trace as Quickstrom

data QuickstromOptions = QuickstromOptions
  { specPath :: FilePath,
    origin :: Text,
    libraryPath :: Maybe FilePath,
    tests :: Int,
    maxActions :: Quickstrom.Size,
    shrinkLevels :: Int,
    maxTrailingStateChanges :: Int,
    logLevel :: Quickstrom.LogLevel
  }

optParser :: Parser QuickstromOptions
optParser =
  QuickstromOptions
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
          ( metavar "DIRECTORY"
              <> long "library-directory"
              <> help "Directory containing compiled PureScript libraries used by Quickstrom (falls back to the WEBCHECK_LIBRARY_DIR environment variable)"
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
      (eitherReader Quickstrom.parseLogLevel)
      ( value Quickstrom.LogInfo
          <> metavar "LEVEL"
          <> long "log-level"
          <> short 'l'
          <> help "Log level used by Quickstrom and the backing WebDriver server (e.g. geckodriver)"
      )

optsInfo :: ParserInfo QuickstromOptions
optsInfo =
  info
    (optParser <**> helper)
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
  QuickstromOptions {..} <- execParser optsInfo
  originUri <- resolveAbsoluteURI origin
  specResult <- runExceptT $ do
    libPath <- maybe libraryPathFromEnvironment pure libraryPath
    modules <- ExceptT (Quickstrom.loadLibraryModules libPath)
    ExceptT (Quickstrom.loadSpecificationFile modules specPath)
  case specResult of
    Left err -> do
      hPutStrLn @Text stderr err
      exitWith (ExitFailure 2)
    Right spec -> flip runReaderT logLevel $ do
      let opts =
            Quickstrom.CheckOptions
              { checkTests = tests,
                checkMaxActions = maxActions,
                checkShrinkLevels = shrinkLevels,
                checkOrigin = originUri,
                checkMaxTrailingStateChanges = maxTrailingStateChanges,
                checkWebDriverLogLevel = logLevel
              }
      result <-
        Pipes.runEffect (Pipes.for (Quickstrom.check opts WebDriver.runWebDriver spec) (lift . logDoc . renderCheckEvent))
          & try
      logDoc . logSingle Nothing $ line <> divider <> line
      case result of
        Right Quickstrom.CheckFailure {failedAfter, failingTest} -> do
          logDoc . logSingle Nothing $
            Quickstrom.prettyTrace (Quickstrom.withoutStutterStates (Quickstrom.trace failingTest))
          case Quickstrom.reason failingTest of
            Just err -> logDoc (logSingle Nothing (line <> annotate (color Red) ("Test failed with error:" <+> pretty err <> line)))
            Nothing -> pure ()
          logDoc . logSingle Nothing . annotate (color Red) $
            line <> "Failed after" <+> pretty failedAfter <+> "tests and" <+> pretty (Quickstrom.numShrinks failingTest) <+> "levels of shrinking." <> line
          liftIO (exitWith (ExitFailure 3))
        Right Quickstrom.CheckError {checkError} -> do
          logDoc . logSingle Nothing . annotate (color Red) $
            line <> "Check encountered an error:" <+> pretty checkError <> line
          liftIO (exitWith (ExitFailure 1))
        Right Quickstrom.CheckSuccess ->
          logDoc . logSingle Nothing . annotate (color Green) $
            line <> "Passed" <+> pretty tests <+> "tests." <> line
        Left err@SomeException {} -> do
          logDoc . logSingle Nothing . annotate (color Red) $
            line <> "Check encountered an error:" <+> pretty (show err :: Text) <> line
          liftIO (exitWith (ExitFailure 1))

libraryPathFromEnvironment :: ExceptT Text IO FilePath
libraryPathFromEnvironment = do
  liftIO (lookupEnv (toS key)) >>= \case
    Just p -> pure p
    Nothing -> throwError (key <> "is not set and command-line option is not provided")
  where
    key = "WEBCHECK_LIBRARY_DIR"

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
    logSingle Nothing $
      annotate bold logo
        <> line
        <> line
        <> ("Running" <+> annotate (color Blue) (pretty n) <+> "tests...")
  Quickstrom.CheckTestEvent e -> renderTestEvent e
  Quickstrom.CheckFinished {} -> mempty

renderTestEvent :: Quickstrom.TestEvent -> [(Maybe Quickstrom.LogLevel, Doc AnsiStyle)]
renderTestEvent = \case
  Quickstrom.TestStarted size ->
    logSingle Nothing $
      line
        <> divider
        <> line
        <> line
        <> annotate bold (renderSize size <+> "Actions")
  Quickstrom.TestPassed size trace' ->
    case traceWarnings size trace' of
      [] ->
        logSingle Nothing $ annotate (color Green) "Test passed!"
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
  Quickstrom.TestFailed _size trace' ->
    logSingle Nothing $
      line
        <> annotate (color Red) "Test failed:"
        <> line
        <> Quickstrom.prettyTrace trace'
  Quickstrom.Shrinking level ->
    logSingle Nothing $
      line
        <> annotate bold ("Shrinking failing test down to the" <+> ordinal level <+> "level..." <> line)
  Quickstrom.RunningShrink level ->
    logSingle (Just Quickstrom.LogInfo) $
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

logSingle :: Maybe Quickstrom.LogLevel -> Doc AnsiStyle -> [(Maybe Quickstrom.LogLevel, Doc AnsiStyle)]
logSingle l d = [(l, d)]

renderSize :: Quickstrom.Size -> Doc AnsiStyle
renderSize (Quickstrom.Size s) = pretty s

renderList :: [Doc ann] -> Doc ann
renderList = vsep . map (\x -> bullet <+> align x)

ordinal :: (Pretty n, Integral n) => n -> Doc ann
ordinal n =
  pretty n <> case n `rem` 10 of
    1 -> "st"
    2 -> "nd"
    3 -> "rd"
    _ -> "th"

logDoc :: (MonadReader Quickstrom.LogLevel m, MonadIO m) => [(Maybe Quickstrom.LogLevel, Doc AnsiStyle)] -> m ()
logDoc logs = for_ logs $ \(logLevel, doc) -> do
  minLogLevel <- ask
  let logAction = putStrLn (renderStrict (layoutPretty defaultLayoutOptions doc))
  case logLevel of
    Just level
      | level >= minLogLevel -> logAction
      | otherwise -> pass
    Nothing -> logAction
