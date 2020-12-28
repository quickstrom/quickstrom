{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Quickstrom.CLI.Reporter.HTML where

import Control.Lens
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Data.Generics.Labels ()
import Data.Generics.Sum (_Ctor)
import Data.HashMap.Strict (HashMap)
import Data.Text.Prettyprint.Doc (pretty, (<+>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Time.Clock as Time
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Quickstrom.CLI.Logging as Quickstrom
import qualified Quickstrom.CLI.Reporter as Quickstrom
import qualified Quickstrom.Element as Quickstrom
import qualified Quickstrom.LogLevel as Quickstrom
import Quickstrom.Prelude hiding (State, uncons)
import qualified Quickstrom.Run as Quickstrom
import qualified Quickstrom.Trace as Quickstrom
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

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

data State = State {name :: Text, screenshot :: Maybe FilePath, queries :: Vector Query}
  deriving (Generic, JSON.ToJSON)

data Query = Query {selector :: Text, elements :: Vector Element}
  deriving (Generic, JSON.ToJSON)

data Element = Element {id :: Text, status :: Status, state :: Vector ElementState}
  deriving (Generic, JSON.ToJSON)

data ElementState
  = Attribute {name :: Text, value :: JSON.Value }
  | Property {name :: Text, value :: JSON.Value }
  | CssValue {name :: Text, value :: JSON.Value }
  | Text { value :: JSON.Value }
  deriving (Generic, JSON.ToJSON)

data Status = Modified
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
      transitions <- traceToTransition reportDir withoutStutters
      -- case Quickstrom.reason failingTest of
      --   Just _err -> pass -- Quickstrom.logDoc (Quickstrom.logSingle Nothing (line <> annotate (color Red) ("Test failed with error:" <+> pretty err <> line)))
      --   Nothing -> pure ()
      pure (Failure {tests = failedAfter, shrinkLevels = Quickstrom.numShrinks failingTest}, Just transitions)
    Quickstrom.CheckError {Quickstrom.checkError} -> do
      pure (Error {error = checkError, tests = Quickstrom.checkTests checkOpts}, Nothing)
    Quickstrom.CheckSuccess -> pure (Success {tests = Quickstrom.checkTests checkOpts}, Nothing)
  let reportFile = reportDir </> "report.json"
      report = Report now summary transitions
  liftIO (JSON.encodeFile reportFile report)

traceToTransition :: (MonadReader Quickstrom.LogLevel m, MonadIO m) => FilePath -> Quickstrom.Trace ann -> m (Vector Transition)
traceToTransition reportDir (Quickstrom.Trace es) = go (Vector.fromList es) mempty
  where
    go :: (MonadReader Quickstrom.LogLevel m, MonadIO m) => Vector (Quickstrom.TraceElement ann) -> Vector Transition -> m (Vector Transition)
    go trace' acc =
      runMaybeT (actionTransition trace' <|> trailingStateTransition trace') >>= \case
        Just (transition, trace'') -> go trace'' (acc <> pure transition)
        Nothing -> pure acc

    actionTransition :: MonadIO m => Vector (Quickstrom.TraceElement ann) -> MaybeT m (Transition, Vector (Quickstrom.TraceElement ann))
    actionTransition t = flip evalStateT t $ do
      s1 <- toState =<< pop (_Ctor @"TraceState" . _2)
      a <- pop (_Ctor @"TraceAction" . _2)
      s2 <- toState =<< pop (_Ctor @"TraceState" . _2)
      rest <- get
      pure (Transition (Just a) (States s1 s2), rest)

    trailingStateTransition :: MonadIO m => Vector (Quickstrom.TraceElement ann) -> MaybeT m (Transition, Vector (Quickstrom.TraceElement ann))
    trailingStateTransition t = flip evalStateT t $ do
      s1 <- toState =<< pop (_Ctor @"TraceState" . _2)
      s2 <- toState =<< pop (_Ctor @"TraceState" . _2)
      rest <- get
      pure (Transition Nothing (States s1 s2), rest)

    toState :: MonadIO m => Quickstrom.ObservedState -> m State
    toState s = do
      screenshot <- case s ^. #screenshot of
        Just s' -> do
          let fileName = (reportDir </> "screenshot-" <> show (hash s') <> ".png")
          liftIO (BS.writeFile fileName s')
          pure (Just fileName)
        Nothing -> pure Nothing
      pure (State "State #?" screenshot (toQueries (s ^. #elementStates)))

    toQueries :: Quickstrom.ObservedElementStates -> Vector Query
    toQueries (Quickstrom.ObservedElementStates os) = Vector.fromList (map toQuery (HashMap.toList os))

    toQuery :: (Quickstrom.Selector, [HashMap Quickstrom.ElementState JSON.Value]) -> Query
    toQuery (Quickstrom.Selector sel, elements') =
      Query {selector = sel, elements = Vector.fromList (map toElement elements')}

    toElement :: HashMap Quickstrom.ElementState JSON.Value -> Element
    toElement states = Element "todo" Modified (Vector.fromList (map toElementState (HashMap.toList states)))

    toElementState :: (Quickstrom.ElementState, JSON.Value) -> ElementState
    toElementState (state', value) = case state' of
        Quickstrom.Attribute n -> Attribute n value
        Quickstrom.Property n -> Property n value
        Quickstrom.CssValue n -> CssValue n value
        Quickstrom.Text -> Text value

    pop ctor = do
      t <- get
      case uncons t of
        Just (a, t') ->
          case a ^? ctor of
            Just x -> put t' >> pure x
            Nothing -> mzero
        Nothing -> mzero
