{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Quickstrom.CLI.Reporter.HTML where

import Control.Lens
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Data.Generics.Labels ()
import Data.Text.Prettyprint.Doc (annotate, line, pretty, (<+>))
import qualified Data.Time.Clock as Time
import Data.Vector (Vector)
import Prettyprinter.Render.Terminal (Color (..), color)
import qualified Quickstrom.CLI.Logging as Quickstrom
import qualified Quickstrom.CLI.Reporter as Quickstrom
import qualified Quickstrom.LogLevel as Quickstrom
import Quickstrom.Prelude hiding (State)
import qualified Quickstrom.Pretty as Quickstrom
import qualified Quickstrom.Run as Quickstrom
import qualified Quickstrom.Trace as Quickstrom
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

data Report = Report
  { generatedAt :: Time.UTCTime,
    summary :: Summary,
    transitions :: Maybe (Vector Transition)
  }
  deriving (Generic, JSON.ToJSON)

data Summary
  = Success {tests :: Int}
  | Failure {tests :: Int, shrinkLevels :: Int}
  | Error {error :: Text, tests :: Int}
  deriving (Generic, JSON.ToJSON)

data Transition = Transition
  { action :: Maybe (Quickstrom.Action Quickstrom.Selected),
    states :: States
  }
  deriving (Generic, JSON.ToJSON)

data States = States {from :: State, to :: State}
  deriving (Generic, JSON.ToJSON)

data State = State {name :: Text, screenshot :: FilePath, queries :: Vector Query}
  deriving (Generic, JSON.ToJSON)

data Query = Query
  deriving (Generic, JSON.ToJSON)

htmlReporter :: (MonadReader Quickstrom.LogLevel m, MonadIO m) => FilePath -> Quickstrom.Reporter m
htmlReporter reportDir _webDriverOpts checkOpts result = do
  now <- liftIO Time.getCurrentTime
  Quickstrom.logDoc . Quickstrom.logSingle Nothing $
    "Writing HTML report to directory:" <+> pretty reportDir
  liftIO (createDirectoryIfMissing True reportDir)
  (summary, transitions) <- case result of
    Quickstrom.CheckFailure {Quickstrom.failedAfter, Quickstrom.failingTest} -> do
      let withoutStutters = Quickstrom.withoutStutterStates (Quickstrom.trace failingTest)
      let screenshots = withoutStutters ^.. Quickstrom.observedStates . #screenshot . _Just
      -- Quickstrom.logDoc (Quickstrom.logSingle Nothing (Quickstrom.prettyTrace withoutStutters))
      void . ifor screenshots $ \i screenshot -> do
        let fileName = (reportDir </> "screenshot-" <> show i <> "-" <> show (hash screenshot) <> ".png")
        liftIO (BS.writeFile fileName screenshot)
      case Quickstrom.reason failingTest of
        Just _err -> pass -- Quickstrom.logDoc (Quickstrom.logSingle Nothing (line <> annotate (color Red) ("Test failed with error:" <+> pretty err <> line)))
        Nothing -> pure ()
      -- Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Red) $
      --   line <> "Failed after" <+> pretty failedAfter <+> "tests and" <+> pretty (Quickstrom.numShrinks failingTest) <+> "levels of shrinking." <> line
      pure (Failure {tests = failedAfter, shrinkLevels = Quickstrom.numShrinks failingTest}, Nothing)
    Quickstrom.CheckError {Quickstrom.checkError} -> do
      -- Quickstrom.logDoc . Quickstrom.logSingle Nothing . annotate (color Red) $
      -- line <> "Check encountered an error:" <+> pretty checkError <> line
      pure (Error {error = checkError, tests = Quickstrom.checkTests checkOpts}, Nothing)
    Quickstrom.CheckSuccess -> pure (Success {tests = Quickstrom.checkTests checkOpts}, Nothing)
  let reportFile = reportDir </> "report.json"
      report = Report now summary transitions
  liftIO (JSON.encodeFile reportFile report)
