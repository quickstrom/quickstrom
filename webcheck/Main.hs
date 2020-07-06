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
import Options.Applicative
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens (uriScheme)
import qualified Text.URI.QQ as URI
import WebCheck.Prelude hiding (option)
import qualified WebCheck.PureScript.Program as WebCheck
import qualified WebCheck.Run as WebCheck

data WebCheckOptions
  = WebCheckOptions
      { specPath :: FilePath,
        origin :: Text,
        libraryPath :: Maybe FilePath,
        tests :: Int,
        shrinkLevels :: Int
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
      ( metavar "ORIGIN"
          <> help "The origin URI"
      )
    <*> optional
      ( strOption
          ( short 'l'
              <> long "library-directory"
              <> help "Directory containing compiled PureScript libraries used by WebCheck (falls back to the WEBCHECK_LIBRARY_DIR environment variable)"
          )
      )
    <*> option
      auto
      ( short 'n'
          <> value 10
          <> long "tests"
          <> help "How many tests to run"
      )
    <*> option
      auto
      ( short 's'
          <> value 10
          <> long "shrink-levels"
          <> help "How many levels to shrink the generated actions after a failed test"
      )

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
    Right spec ->
      WebCheck.check
        WebCheck.CheckOptions {checkTests = tests, checkShrinkLevels = shrinkLevels, checkOrigin = originUri}
        spec

libraryPathFromEnvironment :: ExceptT Text IO FilePath
libraryPathFromEnvironment = do
  liftIO (lookupEnv (toS key)) >>= \case
    Just p -> pure p
    Nothing -> throwError (key <> "is not set and command-line option is not provided")
  where
    key = "WEBCHECK_LIBRARY_DIR"
{-

-- Simple example: a button that can be clicked, which then shows a message
buttonSpec :: Specification Formula
buttonSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/button.html"),
      readyWhen = "button",
      actions = clicks,
      proposition =
        let click = buttonIsEnabled /\ next (".message" `hasText` "Boom!" /\ neg buttonIsEnabled)
         in buttonIsEnabled /\ always click
    }

ajaxSpec :: Specification Formula
ajaxSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/ajax.html"),
      readyWhen = "button",
      actions = clicks,
      proposition =
        let disabledLaunchWithMessage msg =
              ".message" `hasText` msg /\ neg buttonIsEnabled
            launch =
              buttonIsEnabled
                /\ next (disabledLaunchWithMessage "Missiles launched.")
            impactOrNoImpact =
              disabledLaunchWithMessage "Missiles launched."
                /\ next
                  ( disabledLaunchWithMessage "Boom"
                      \/ disabledLaunchWithMessage "Missiles did not hit target."
                  )
         in buttonIsEnabled /\ always (launch \/ impactOrNoImpact)
    }

draftsSpec :: Specification Formula
draftsSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/drafts.html"),
      readyWhen = "button",
      actions = clicks,
      proposition = top
    }
-}
