{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens hiding (argument)
import qualified Options.Applicative as Options
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import qualified Data.Text.Lazy as TL
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens (uriScheme)
import qualified Text.URI.QQ as URI
import Web.Scotty.Trans
import WebCheck.Prelude hiding (get, option)
import qualified WebCheck.PureScript.Program as WebCheck
import qualified WebCheck.Run as WebCheck

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

data Env = Env { modules :: WebCheck.Modules, webOptions :: WebOptions }

type App = ScottyT TL.Text (ReaderT Env IO) ()

app :: App
app =
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

-- originUri <- resolveAbsoluteURI origin
-- ExceptT (WebCheck.loadSpecificationFile modules specPath)
-- case specResult of
-- Left err -> do
-- hPutStrLn @Text stderr err
-- exitWith (ExitFailure 1)
-- Right spec ->
-- WebCheck.check
-- WebCheck.CheckOptions {checkTests = tests, checkShrinkLevels = shrinkLevels, checkOrigin = originUri}
-- spec

main :: IO ()
main = do
  webOptions <- Options.execParser optsInfo
  modulesResult <- runExceptT $ do
    libPath <- maybe libraryPathFromEnvironment pure (libraryPath webOptions)
    ExceptT (WebCheck.loadLibraryModules libPath)
  case modulesResult of
    Left err -> do
      hPutStrLn @Text stderr err
      exitWith (ExitFailure 1)
    Right modules -> do
      let env = Env { modules, webOptions }
      scottyT 3000 (flip runReaderT env) app

libraryPathFromEnvironment :: ExceptT Text IO FilePath
libraryPathFromEnvironment = do
  liftIO (lookupEnv (toS key)) >>= \case
    Just p -> pure p
    Nothing -> throwError (key <> "is not set and command-line option is not provided")
  where
    key = "WEBCHECK_LIBRARY_DIR"
