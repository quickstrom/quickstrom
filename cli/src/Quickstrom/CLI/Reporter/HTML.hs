{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
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
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Data.Generics.Labels ()
import Data.Generics.Sum (_Ctor)
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
    transitions :: Maybe (Vector (Transition FilePath))
  }
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Summary
  = Success {tests :: Int}
  | Failure {tests :: Int, shrinkLevels :: Int}
  | Error {error :: Text, tests :: Int}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Transition screenshot = Transition
  { action :: Maybe (Quickstrom.Action Quickstrom.Selected),
    states :: States screenshot
  }
  deriving (Eq, Show, Generic, JSON.ToJSON, Functor, Foldable, Traversable)

data States screenshot = States {from :: State screenshot, to :: State screenshot}
  deriving (Eq, Show, Generic, JSON.ToJSON, Functor, Foldable, Traversable)

data State screenshot = State {screenshot :: Maybe screenshot, queries :: Vector Query}
  deriving (Eq, Show, Generic, JSON.ToJSON, Functor, Foldable, Traversable)

data Query = Query {selector :: Text, elements :: Vector Element}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Element = Element {id :: Text, status :: Status, state :: Vector ElementState}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data ElementState
  = Attribute {name :: Text, value :: JSON.Value}
  | Property {name :: Text, value :: JSON.Value}
  | CssValue {name :: Text, value :: JSON.Value}
  | Text {value :: JSON.Value}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Status = Modified
  deriving (Eq, Show, Generic, JSON.ToJSON)

htmlReporter :: (MonadReader Quickstrom.LogLevel m, MonadIO m) => FilePath -> Quickstrom.Reporter m
htmlReporter reportDir _webDriverOpts checkOpts result = do
  now <- liftIO Time.getCurrentTime
  Quickstrom.logDoc . Quickstrom.logSingle Nothing $
    "Writing HTML report to directory:" <+> pretty reportDir
  liftIO (createDirectoryIfMissing True reportDir)
  (summary, transitions) <- case result of
    Quickstrom.CheckFailure {Quickstrom.failedAfter, Quickstrom.failingTest} -> do
      let -- withoutStutters = Quickstrom.withoutStutterStates (Quickstrom.trace failingTest)
          transitions = traceToTransition (Quickstrom.trace failingTest)
      -- case Quickstrom.reason failingTest of
      --   Just _err -> pass -- Quickstrom.logDoc (Quickstrom.logSingle Nothing (line <> annotate (color Red) ("Test failed with error:" <+> pretty err <> line)))
      --   Nothing -> pure ()
      pure (Failure {tests = failedAfter, shrinkLevels = Quickstrom.numShrinks failingTest}, Just transitions)
    Quickstrom.CheckError {Quickstrom.checkError} -> do
      pure (Error {error = checkError, tests = Quickstrom.checkTests checkOpts}, Nothing)
    Quickstrom.CheckSuccess -> pure (Success {tests = Quickstrom.checkTests checkOpts}, Nothing)
  let reportFile = reportDir </> "report.json"
  report <- Report now summary <$> traverse (writeScreenshotFiles reportDir) transitions
  liftIO (JSON.encodeFile reportFile report)

traceToTransition :: Quickstrom.Trace ann -> Vector (Transition ByteString)
traceToTransition (Quickstrom.Trace es) = go (Vector.fromList es) mempty
  where
    go :: Vector (Quickstrom.TraceElement ann) -> Vector (Transition ByteString) -> Vector (Transition ByteString)
    go trace' acc =
      case actionTransition trace' <|> trailingStateTransition trace' of
        Just (transition, trace'') -> go trace'' (acc <> pure transition)
        Nothing -> acc

    actionTransition :: Vector (Quickstrom.TraceElement ann) -> Maybe (Transition ByteString, Vector (Quickstrom.TraceElement ann))
    actionTransition t = flip evalStateT t $ do
      s1 <- toState <$> pop (_Ctor @"TraceState" . _2)
      a <- pop (_Ctor @"TraceAction" . _2)
      s2 <- toState <$> pop (_Ctor @"TraceState" . _2)
      rest <- get
      pure (Transition (Just a) (States s1 s2), rest)

    trailingStateTransition :: Vector (Quickstrom.TraceElement ann) -> Maybe (Transition ByteString, Vector (Quickstrom.TraceElement ann))
    trailingStateTransition t = flip evalStateT t $ do
      s1 <- toState <$> pop (_Ctor @"TraceState" . _2)
      s2 <- toState <$> pop (_Ctor @"TraceState" . _2)
      rest <- get
      pure (Transition Nothing (States s1 s2), rest)

    toState :: Quickstrom.ObservedState -> State ByteString
    toState s = State (s ^. #screenshot) (toQueries (s ^. #elementStates))

    toQueries :: Quickstrom.ObservedElementStates -> Vector Query
    toQueries (Quickstrom.ObservedElementStates os) = Vector.fromList (map toQuery (HashMap.toList os))

    toQuery :: (Quickstrom.Selector, [Quickstrom.ObservedElementState]) -> Query
    toQuery (Quickstrom.Selector sel, elements') =
      Query {selector = sel, elements = Vector.fromList (map toElement elements')}

    toElement :: Quickstrom.ObservedElementState -> Element
    toElement o = Element (o ^. #element . #ref) Modified (Vector.fromList (map toElementState (HashMap.toList (o ^. #elementState))))

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

writeScreenshotFiles :: MonadIO m => FilePath -> Vector (Transition ByteString) -> m (Vector (Transition FilePath))
writeScreenshotFiles reportDir =
  traverse
    ( traverse $ \s -> do
        let fileName = (reportDir </> "screenshot-" <> show (hash s) <> ".png")
        liftIO (BS.writeFile fileName s)
        pure fileName
    )
