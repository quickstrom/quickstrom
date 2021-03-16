{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Quickstrom.CLI.Reporter.HTML where

import qualified Codec.Picture as Image
import Control.Lens hiding (Identical)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import "base64" Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Labels ()
import Data.Generics.Sum (_Ctor)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc (pretty, (<+>))
import qualified Data.Time.Clock as Time
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Quickstrom.Action as Quickstrom
import qualified Quickstrom.CLI.Reporter as Quickstrom
import qualified Quickstrom.Element as Quickstrom
import qualified Quickstrom.LogLevel as Quickstrom
import Quickstrom.Prelude hiding (State, uncons)
import qualified Quickstrom.Run as Quickstrom
import qualified Quickstrom.Trace as Quickstrom
import qualified System.Directory as Directory
import System.Environment (getEnv)
import System.FilePath ((</>))

data Report = Report
  { generatedAt :: Time.UTCTime,
    result :: Result
  }
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Result
  = Passed {passedTests :: Vector Test}
  | Failed
      { numShrinks :: Int,
        reason :: Maybe Text,
        passedTests :: Vector Test,
        failedTest :: Test
      }
  | Errored {error :: Text, tests :: Int}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Test = Test {transitions :: Transitions}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Transition screenshot = Transition
  { actionSequence :: Maybe (Quickstrom.ActionSequence Quickstrom.ActionSubject),
    states :: States screenshot,
    stutter :: Bool
  }
  deriving (Eq, Show, Generic, JSON.ToJSON, Functor, Foldable, Traversable)

type Transitions = Vector (Transition FileScreenshot)

data States screenshot = States {from :: State screenshot, to :: State screenshot}
  deriving (Eq, Show, Generic, JSON.ToJSON, Functor, Foldable, Traversable)

data State screenshot = State {screenshot :: Maybe screenshot, queries :: Vector Query}
  deriving (Eq, Show, Generic, JSON.ToJSON, Functor, Foldable, Traversable)

data Base64Screenshot = Base64Screenshot {encoded :: Text, width :: Int, height :: Int}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data FileScreenshot = FileScreenshot {url :: FilePath, width :: Int, height :: Int}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Query = Query {selector :: Text, elements :: Vector QueriedElement}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data QueriedElement = QueriedElement {id :: Text, position :: Maybe Quickstrom.Position, state :: Vector ElementStateValue}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data ActionElement = ActionElement {id :: Text, position :: Maybe Quickstrom.Position}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data ElementStateValue = ElementStateValue {elementState :: Quickstrom.ElementState, value :: JSON.Value, diff :: Diff}
  deriving (Eq, Show, Generic, JSON.ToJSON)

data Diff = Identical | Modified | Removed | Added
  deriving (Eq, Show, Generic, JSON.ToJSON)

type ElementStateDiffs = HashMap (Quickstrom.Element, Quickstrom.ElementState) Diff

elementStateDiffs :: Quickstrom.ObservedState -> Quickstrom.ObservedState -> ElementStateDiffs
elementStateDiffs s1 s2 =
  let vs1 = fromObservedState s1
      vs2 = fromObservedState s2
      allKeys = HashMap.keys (HashMap.union vs1 vs2)
   in HashMap.fromList
        ( map
            ( \k ->
                case (HashMap.lookup k vs1, HashMap.lookup k vs2) of
                  (Just v1, Just v2)
                    | v1 == v2 -> (k, Identical)
                    | otherwise -> (k, Modified)
                  (Nothing, Just _) -> (k, Added)
                  (Just _, Nothing) -> (k, Removed)
                  (Nothing, Nothing) -> (k, Identical) -- absurd case
            )
            allKeys
        )
  where
    fromObservedState :: Quickstrom.ObservedState -> HashMap (Quickstrom.Element, Quickstrom.ElementState) JSON.Value
    fromObservedState s = foldMap fromElement (s ^.. #elementStates . _Wrapped' . folded . folded)
    fromElement :: Quickstrom.ObservedElementState -> HashMap (Quickstrom.Element, Quickstrom.ElementState) JSON.Value
    fromElement oes =
      let element' = oes ^. #element
       in HashMap.fromList [((element', es), value) | (es, value) <- HashMap.toList (oes ^. #elementState)]

data HTMLReporterException = HTMLReporterException Text
  deriving (Show, Eq)

instance Exception HTMLReporterException

htmlReporter :: (MonadReader Quickstrom.LogLevel m, MonadIO m) => FilePath -> Quickstrom.Reporter m
htmlReporter reportDir = Quickstrom.Reporter {preCheck, report}
  where
    preCheck _webDriverOpts _checkOpts = do
      alreadyExists <- liftIO (Directory.doesPathExist reportDir)
      if alreadyExists
        then pure (Quickstrom.CannotBeInvoked ("File or directory already exists, refusing to overwrite:" <+> pretty reportDir))
        else pure Quickstrom.OK

    report _webDriverOpts checkOpts result = do
      now <- liftIO Time.getCurrentTime

      whenM (liftIO (Directory.doesPathExist reportDir)) $
        throwIO (HTMLReporterException "File or directory already exists, refusing to overwrite!")

      liftIO (Directory.createDirectoryIfMissing True reportDir)
      reportResult <- case result of
        Quickstrom.CheckFailure {Quickstrom.passedTests, Quickstrom.failedTest} -> do
          passedTests' <- traverse (traceToTest reportDir . view #trace) passedTests
          failedTest' <- traceToTest reportDir (failedTest ^. #trace)
          pure
            Failed
              { numShrinks = Quickstrom.numShrinks failedTest,
                reason = Quickstrom.reason failedTest,
                passedTests = passedTests',
                failedTest = failedTest'
              }
        Quickstrom.CheckError {Quickstrom.checkError} -> do
          pure Errored {error = checkError, tests = Quickstrom.checkTests checkOpts}
        Quickstrom.CheckSuccess {passedTests} -> do
          passedTests' <- traverse (traceToTest reportDir . view #trace) passedTests
          pure Passed {passedTests = passedTests'}
      let reportFile = reportDir </> "report.jsonp.js"
          json = JSON.encode (Report now reportResult)
      liftIO $ do
        BS.writeFile reportFile (Text.encodeUtf8 "window.report = " <> LBS.toStrict json)
        getAssets >>= \case
          Just assets | not (null assets) ->
            for_ assets $ \(name, contents) ->
              BS.writeFile (reportDir </> name) contents
          _ -> throwIO (HTMLReporterException "HTML report assets not found.")

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
      let diffs = elementStateDiffs s1 s2
      pure (Transition (Just a) (States (toState diffs s1) (toState diffs s2)) (ann2 == Quickstrom.Stutter), Vector.drop 2 t)

    trailingStateTransition :: Vector (Quickstrom.TraceElement Quickstrom.TraceElementEffect) -> Maybe (Transition ByteString, Vector (Quickstrom.TraceElement Quickstrom.TraceElementEffect))
    trailingStateTransition t = flip evalStateT t $ do
      (_, s1) <- pop (_Ctor @"TraceState")
      (ann2, s2) <- pop (_Ctor @"TraceState")
      let diffs = elementStateDiffs s1 s2
      pure (Transition Nothing (States (toState diffs s1) (toState diffs s2)) (ann2 == Quickstrom.Stutter), Vector.tail t)

    toState :: ElementStateDiffs -> Quickstrom.ObservedState -> State ByteString
    toState diffs s = State (s ^. #screenshot) (toQueries diffs (s ^. #elementStates))

    toQueries :: ElementStateDiffs -> Quickstrom.ObservedElementStates -> Vector Query
    toQueries diffs (Quickstrom.ObservedElementStates os) = Vector.fromList (map (toQuery diffs) (HashMap.toList os))

    toQuery :: ElementStateDiffs -> (Quickstrom.Selector, [Quickstrom.ObservedElementState]) -> Query
    toQuery diffs (Quickstrom.Selector sel, elements') =
      Query {selector = sel, elements = Vector.fromList (map (toQueriedElement diffs) elements')}

    toQueriedElement :: ElementStateDiffs -> Quickstrom.ObservedElementState -> QueriedElement
    toQueriedElement diffs o =
      QueriedElement
        (o ^. #element . #ref)
        (o ^. #position)
        (Vector.fromList (map (toElementStateValue diffs (o ^. #element)) (HashMap.toList (o ^. #elementState))))

    toElementStateValue :: ElementStateDiffs -> Quickstrom.Element -> (Quickstrom.ElementState, JSON.Value) -> ElementStateValue
    toElementStateValue diffs element' (state', value) =
      ElementStateValue state' value (fromJust (HashMap.lookup (element', state') diffs))

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
  let fileName = "screenshot-" <> show (hash s) <> ".png"
  liftIO (BS.writeFile (reportDir </> fileName) s)
  either
    (throwIO . ScreenshotFileException . toS)
    (pure . Image.dynamicMap (\i -> FileScreenshot fileName (Image.imageWidth i) (Image.imageHeight i)))
    (Image.decodePng s)

traceToTest :: MonadIO m => FilePath -> Quickstrom.Trace Quickstrom.TraceElementEffect -> m Test
traceToTest reportDir trace' =
  traceToTransitions trace'
    & traverse . traverse %%~ writeScreenshotFile reportDir
    & fmap Test

getAssets :: MonadIO m => m (Maybe [(FilePath, ByteString)])
getAssets = liftIO $ do
  path <- getEnv "QUICKSTROM_HTML_REPORT_DIR"
  files <-
    Directory.listDirectory path
      >>= traverse (\fileName -> (fileName,) <$> BS.readFile (path </> fileName))
  pure (Just files)
