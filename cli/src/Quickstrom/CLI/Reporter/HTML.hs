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
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Quickstrom.CLI.Reporter.HTML where

import qualified Codec.Picture as Image
import Control.Lens
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import "base64" Data.ByteString.Base64 as Base64
import Data.FileEmbed (embedDir, makeRelativeToProject)
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Labels ()
import Data.Generics.Sum (_Ctor)
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Prettyprint.Doc (pretty, (<+>))
import qualified Data.Text.Encoding as Text
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
    transitions :: Maybe (Vector (Transition FileScreenshot))
  }
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Summary
  = Success {tests :: Int}
  | Failure {tests :: Int, shrinkLevels :: Int}
  | Error {error :: Text, tests :: Int}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Transition screenshot = Transition
  { action :: Maybe (Quickstrom.Action Quickstrom.Selected),
    states :: States screenshot,
    stutter :: Bool
  }
  deriving (Eq, Show, Generic, JSON.ToJSON, Functor, Foldable, Traversable)

data States screenshot = States {from :: State screenshot, to :: State screenshot}
  deriving (Eq, Show, Generic, JSON.ToJSON, Functor, Foldable, Traversable)

data State screenshot = State {screenshot :: Maybe screenshot, queries :: Vector Query}
  deriving (Eq, Show, Generic, JSON.ToJSON, Functor, Foldable, Traversable)

data Base64Screenshot = Base64Screenshot {encoded :: Text, width :: Int, height :: Int}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data FileScreenshot = FileScreenshot {url :: FilePath, width :: Int, height :: Int}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Query = Query {selector :: Text, elements :: Vector Element}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Element = Element {id :: Text, status :: Status, position :: Maybe Quickstrom.Position, state :: Vector ElementState}
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
      let transitions = traceToTransitions (Quickstrom.trace failingTest)
      -- case Quickstrom.reason failingTest of
      --   Just _err -> pass -- Quickstrom.logDoc (Quickstrom.logSingle Nothing (line <> annotate (color Red) ("Test failed with error:" <+> pretty err <> line)))
      --   Nothing -> pure ()
      pure (Failure {tests = failedAfter, shrinkLevels = Quickstrom.numShrinks failingTest}, Just transitions)
    Quickstrom.CheckError {Quickstrom.checkError} -> do
      pure (Error {error = checkError, tests = Quickstrom.checkTests checkOpts}, Nothing)
    Quickstrom.CheckSuccess -> pure (Success {tests = Quickstrom.checkTests checkOpts}, Nothing)
  let reportFile = reportDir </> "report.jsonp"
  transitions & traverse . traverse . traverse %%~ writeScreenshotFile reportDir >>= \case
    transitionsWithScreenshots -> do
      let json = JSON.encode (Report now summary transitionsWithScreenshots)
      liftIO $ do
        BS.writeFile reportFile (Text.encodeUtf8 "window.report = " <> LBS.toStrict json)
        for_ assets $ \(name, contents) ->
          BS.writeFile (reportDir </> name) contents

encodeScreenshot :: ByteString -> Either Text Base64Screenshot
encodeScreenshot b =
  let b64 = Base64.encodeBase64 b
   in bimap
        toS
        (Image.dynamicMap (\i -> Base64Screenshot b64 (Image.imageWidth i) (Image.imageHeight i)))
        (Image.decodePng b)

traceToTransitions :: Quickstrom.Trace Quickstrom.TraceElementEffect -> Vector (Transition ByteString)
traceToTransitions (Quickstrom.Trace es) = go (Vector.fromList es) mempty
  where
    go :: Vector (Quickstrom.TraceElement Quickstrom.TraceElementEffect) -> Vector (Transition ByteString) -> Vector (Transition ByteString)
    go trace' acc =
      case actionTransition trace' <|> trailingStateTransition trace' of
        Just (transition, trace'') -> go trace'' (acc <> pure transition)
        Nothing -> acc

    actionTransition :: Vector (Quickstrom.TraceElement Quickstrom.TraceElementEffect) -> Maybe (Transition ByteString, Vector (Quickstrom.TraceElement Quickstrom.TraceElementEffect))
    actionTransition t = flip evalStateT t $ do
      (_, s1) <- pop (_Ctor @"TraceState")
      a <- pop (_Ctor @"TraceAction" . _2)
      (ann2, s2) <- pop (_Ctor @"TraceState")
      pure (Transition (Just a) (States (toState s1) (toState s2)) (ann2 == Quickstrom.Stutter), (Vector.drop 2 t))

    trailingStateTransition :: Vector (Quickstrom.TraceElement Quickstrom.TraceElementEffect) -> Maybe (Transition ByteString, Vector (Quickstrom.TraceElement Quickstrom.TraceElementEffect))
    trailingStateTransition t = flip evalStateT t $ do
      (_, s1) <- pop (_Ctor @"TraceState")
      (ann2, s2) <- pop (_Ctor @"TraceState")
      pure (Transition Nothing (States (toState s1) (toState s2)) (ann2 == Quickstrom.Stutter), Vector.tail t)

    toState :: Quickstrom.ObservedState -> State ByteString
    toState s = State (s ^. #screenshot) (toQueries (s ^. #elementStates))

    toQueries :: Quickstrom.ObservedElementStates -> Vector Query
    toQueries (Quickstrom.ObservedElementStates os) = Vector.fromList (map toQuery (HashMap.toList os))

    toQuery :: (Quickstrom.Selector, [Quickstrom.ObservedElementState]) -> Query
    toQuery (Quickstrom.Selector sel, elements') =
      Query {selector = sel, elements = Vector.fromList (map toElement elements')}

    toElement :: Quickstrom.ObservedElementState -> Element
    toElement o =
      Element
        (o ^. #element . #ref)
        Modified
        (o ^. #position)
        (Vector.fromList (map toElementState (HashMap.toList (o ^. #elementState))))

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

data ScreenshotFileException = ScreenshotFileException Text
  deriving (Show)

instance Exception ScreenshotFileException

writeScreenshotFile :: MonadIO m => FilePath -> ByteString -> m FileScreenshot
writeScreenshotFile reportDir s = do
  let fileName = ("screenshot-" <> show (hash s) <> ".png")
  liftIO (BS.writeFile (reportDir </> fileName) s)
  either
    (throwIO . ScreenshotFileException . toS)
    (pure . Image.dynamicMap (\i -> FileScreenshot fileName (Image.imageWidth i) (Image.imageHeight i)))
    (Image.decodePng s)

-- * Static assets

assets :: [(FilePath, ByteString)]
assets = $(makeRelativeToProject "src/Quickstrom/CLI/Reporter/HTML" >>= embedDir)
