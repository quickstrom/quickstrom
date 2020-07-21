{-# LANGUAGE BlockArguments #-}
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
import qualified Data.Text.Lazy as TL
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Lucid
import qualified Options.Applicative as Options
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens (uriScheme)
import qualified Text.URI.QQ as URI
import Web.Scotty.Trans
import WebCheck.Prelude hiding (get, option)
import qualified WebCheck.PureScript.Program as WebCheck
import qualified WebCheck.Run as WebCheck
import qualified WebCheck.Trace as WebCheck

data Env = Env {modules :: WebCheck.Modules, webOptions :: WebOptions}

type App = ScottyT TL.Text (ReaderT Env IO) ()

layout :: Text -> Html () -> Html ()
layout title content = do
  html_ do
    head_ do
      title_ (toHtml title)
    body_ do
      content

homeView :: Html ()
homeView = layout "WebCheck" do
  h1_ "WebCheck"
  form_ [action_ "/checks", method_ "POST"] do
    label_ do
      "Origin URL"
      input_ [type_ "url", name_ "origin", placeholder_ "https://example.com"] 
    label_ do
      "Specification"
      textarea_ [name_ "spec"] do
        "module Specification where"
        "\n\n"
        "import WebCheck"
        "\n\n"
        "readyWhen = \"body\""
        "\n\n"
        "actions = clicks"
        "\n\n"
        "proposition = ?todo"

checkResultsView :: Html () -> Html ()
checkResultsView content = layout "Check Results" do
  h1_ "Check Results"
  content

app :: App
app = do
  get "/" do
    renderHtml homeView
  post "/checks" do
    env <- ask
    origin <- param "origin"
    specCode <- param "spec"
    originUri <- liftAndCatchIO (resolveAbsoluteURI origin)
    specResult <- liftAndCatchIO (WebCheck.loadSpecification (modules env) specCode)
    case specResult of
      Left err -> 
        renderHtml $ checkResultsView do
          p_ do
            "Check error:"
            pre_ (code_ (toHtml err))
      Right spec -> do
        let WebOptions {..} = webOptions env
        result <-
          liftAndCatchIO $
            WebCheck.check
              WebCheck.CheckOptions {checkTests = tests, checkShrinkLevels = shrinkLevels, checkOrigin = originUri}
              spec
        case result of
          WebCheck.CheckFailure {failedAfter, failingTest} -> do
            let _trace' = (WebCheck.withoutStutterStates (WebCheck.trace failingTest))
            renderHtml $ checkResultsView do
              p_ do
                "Check failed after "
                toHtml @Text (show failedAfter)
                "tests and"
                toHtml @Text (show (WebCheck.numShrinks failingTest))
                "levels of shrinking."
              case WebCheck.reason failingTest of
                Just err -> p_ (toHtml (renderString (unAnnotate err)))
                Nothing -> mempty
          WebCheck.CheckSuccess -> text "Check passed."

main :: IO ()
main = do
  webOptions <- Options.execParser optsInfo
  modulesResult <- runExceptT do
    libPath <- maybe libraryPathFromEnvironment pure (libraryPath webOptions)
    ExceptT (WebCheck.loadLibraryModules libPath)
  case modulesResult of
    Left err -> do
      hPutStrLn @Text stderr err
      exitWith (ExitFailure 1)
    Right modules -> do
      let env = Env {modules, webOptions}
      scottyT 3000 (flip runReaderT env) app

renderHtml :: Monad m => Html () -> ActionT TL.Text m ()
renderHtml = html . renderText

renderString :: Doc () -> TL.Text
renderString = toS . renderStrict . layoutPretty defaultLayoutOptions

-- * Options

data WebOptions
  = WebOptions
      { libraryPath :: Maybe FilePath,
        tests :: Int,
        shrinkLevels :: Int
      }

optParser :: Options.Parser WebOptions
optParser =
  WebOptions
    <$> optional
      ( Options.strOption
          ( Options.short 'l'
              <> Options.long "library-directory"
              <> Options.help "Directory containing compiled PureScript libraries used by WebCheck (falls back to the WEBCHECK_LIBRARY_DIR environment variable)"
          )
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

optsInfo :: Options.ParserInfo WebOptions
optsInfo =
  Options.info
    (optParser <**> Options.helper)
    ( Options.fullDesc
        <> Options.header "WebCheck: High-confidence browser testing"
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
    key = "WEBCHECK_LIBRARY_DIR"
