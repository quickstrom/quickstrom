{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Quickstrom.Run
  ( WebDriver (..),
    WebDriverResponseError (..),
    WebDriverOtherError (..),
    CheckEnv (..),
    CheckOptions (..),
    CheckResult (..),
    checkResultActions,
    PassedTest (..),
    FailedTest (..),
    Size (..),
    CheckEvent (..),
    TestEvent (..),
    check,
  )
where

import Control.Lens hiding (each)
import Control.Monad (fail)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Generics.Product (field)
import qualified Data.Generics.Product as Product
import Data.Generics.Sum (_As, _Ctor)
import Data.List hiding (map, sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Pipes (Pipe, Producer, (>->))
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import Quickstrom.Action
import Quickstrom.Prelude hiding (Prefix, catch, check, trace)
import Quickstrom.Result
import Quickstrom.Run.Actions (Selectable, awaitElement, defaultTimeout, generateValidActions, isCurrentlyValid, runActionSequence, selectOne)
import Quickstrom.Run.Runner (CheckEnv (..), CheckOptions (..), Runner, Size (..), run)
import Quickstrom.Run.Scripts (CheckScripts (..), readScripts, runCheckScript)
import Quickstrom.Run.Shrinking
import Quickstrom.Specification
import Quickstrom.Timeout (Timeout, mapTimeout)
import Quickstrom.Trace
import Quickstrom.WebDriver.Class
import qualified Text.URI as URI

newtype PassedTest = PassedTest
  { trace :: Trace TraceElementEffect
  }
  deriving (Show, Generic)

data FailedTest = FailedTest
  { failedAfterShrinks :: [ShrinkResult],
    trace :: Trace TraceElementEffect,
    reason :: Maybe Text
  }
  deriving (Show, Generic)

data CheckResult
  = CheckSuccess {passedTests :: Vector PassedTest}
  | CheckFailure {failedAfter :: Int, passedTests :: Vector PassedTest, failedTest :: FailedTest}
  | CheckError {checkError :: Text}
  deriving (Show, Generic)

checkResultActions :: CheckResult -> Vector (ActionSequence ActionSubject ActionSubject)
checkResultActions = \case
  CheckSuccess {passedTests} -> Vector.fromList (passedTests ^.. traverse . field @"trace" . traceActions)
  CheckFailure {passedTests, failedTest} ->
    Vector.fromList
      ( passedTests ^.. traverse . field @"trace" . traceActions
          <> failedTest ^.. field @"trace" . traceActions
          <> failedTest ^.. field @"failedAfterShrinks" . traverse . _Ctor @"ShrinkTestFailure" . field @"trace" . traceActions
      )
  CheckError {} -> mempty

-- <> Vector.fromList (r ^.. _Ctor @"CheckFailure" . Product.position @3 . field @"trace" . traceActions)

data TestEvent
  = TestStarted Size
  | TestPassed Size (Trace TraceElementEffect)
  | TestFailed Size (Trace TraceElementEffect)
  | Shrinking Int
  | RunningShrink Int
  deriving (Show, Generic)

data CheckEvent
  = CheckStarted Int
  | CheckTestEvent TestEvent
  | CheckFinished CheckResult
  deriving (Show, Generic)

data ShrinkResult
  = ShrinkTestSuccess {shrunkTrace :: Trace TraceElementEffect}
  | ShrinkTestFailure {failedTest :: FailedTest}
  | ShrinkTestError {shrinkError :: Text}
  deriving (Show, Generic)

check ::
  (Specification spec, WebDriver m, MonadIO m, MonadCatch m) =>
  CheckOptions ->
  spec ->
  Pipes.Producer CheckEvent m CheckResult
check opts@CheckOptions {checkTests} spec = do
  -- stdGen <- getStdGen
  Pipes.yield (CheckStarted checkTests)
  env <- CheckEnv opts <$> lift readScripts
  res <-
    Pipes.hoist (run env) (runAll opts spec)
      & (`catch` \err@SomeException {} -> pure (CheckError (show err)))
  Pipes.yield (CheckFinished res)
  pure res

elementsToTrace :: Monad m => Producer (TraceElement ()) (Runner m) () -> Runner m (Trace ())
elementsToTrace = fmap Trace . Pipes.toListM

runSingle ::
  (MonadIO m, MonadCatch m, WebDriver m, Specification spec) =>
  spec ->
  Size ->
  Producer TestEvent (Runner m) (Either FailedTest PassedTest)
runSingle spec size = do
  Pipes.yield (TestStarted size)
  result <-
    generateValidActions (actions spec)
      >-> Pipes.take (fromIntegral (unSize size))
      & runAndVerifyIsolated
  case result of
    Right trace -> do
      Pipes.yield (TestPassed size trace)
      pure (Right (PassedTest trace))
    Left ft@(FailedTest _ trace _) -> do
      CheckOptions {checkMaxShrinks} <- lift (asks checkOptions)
      Pipes.yield (TestFailed size trace)
      if checkMaxShrinks > 0
        then do
          Pipes.yield (Shrinking checkMaxShrinks)
          let prefixes = shrinkPrefixes (map (bimap (view #selected) (view #selected)) (trace ^.. traceActions))
          counterExamples <-
            Pipes.toListM
              ( searchSmallestFailingPrefix runShrink (_Ctor @"ShrinkTestFailure") prefixes
                  >-> Pipes.take checkMaxShrinks
              )
          let counterExample =
                counterExamples
                  & minimumByOf (folded . _Ctor @"ShrinkTestFailure") (compare `on` lengthOf (field @"trace" . traceElements))
          pure (maybe (Left ft) (Left . (field @"failedAfterShrinks" .~ counterExamples)) counterExample)
        else pure (Left ft)
  where
    runAndVerifyIsolated ::
      (MonadIO m, MonadCatch m, WebDriver m, Selectable s, Show s) =>
      Producer (ActionSequence s ActionSubject) (Runner m) () ->
      Producer TestEvent (Runner m) (Either FailedTest (Trace TraceElementEffect))
    runAndVerifyIsolated producer = do
      trace <- lift do
        opts <- asks checkOptions
        annotateStutteringSteps <$> inNewPrivateWindow (checkWebDriverOptions opts) do
          beforeRun spec
          elementsToTrace (producer >-> runActions' spec)
      case verify spec (trace ^.. nonStutterStates) of
        Right Accepted -> pure (Right trace)
        Right Rejected -> pure (Left (FailedTest mempty trace Nothing))
        Left err -> pure (Left (FailedTest mempty trace (Just err)))
    runShrink ::
      (MonadIO m, MonadCatch m, WebDriver m) =>
      Prefix (ActionSequence Selected Selected) ->
      Producer TestEvent (Runner m) ShrinkResult
    runShrink (Prefix actions') = do
      Pipes.yield (RunningShrink (length actions'))
      Pipes.each actions'
        >-> Pipes.mapM (traverse (maybe (fail "Couldn't reselect the shrunk action") pure <=< selectOne))
        >-> Pipes.filterM isCurrentlyValid
        & runAndVerifyIsolated
        & (<&> either ShrinkTestFailure ShrinkTestSuccess)
        & (`catch` (\(WebDriverOtherError t) -> pure (ShrinkTestError t)))

runAll :: (MonadIO m, MonadCatch m, WebDriver m, Specification spec) => CheckOptions -> spec -> Producer CheckEvent (Runner m) CheckResult
runAll opts spec' = go mempty (sizes opts `zip` [1 ..])
  where
    go :: (MonadIO m, MonadCatch m, WebDriver m) => Vector PassedTest -> [(Size, Int)] -> Producer CheckEvent (Runner m) CheckResult
    go passed [] = pure (CheckSuccess passed)
    go passed ((size, n) : rest) =
      (runSingle spec' size >-> Pipes.map CheckTestEvent)
        >>= \case
          Right passedTest -> go (passed <> pure passedTest) rest
          Left failingTest -> pure (CheckFailure n passed failingTest)

sizes :: CheckOptions -> [Size]
sizes CheckOptions {checkMaxActions = Size maxActions, checkTests} =
  map (\n -> Size (n * maxActions `div` fromIntegral checkTests)) [1 .. fromIntegral checkTests]

navigateToOrigin :: WebDriver m => Runner m ()
navigateToOrigin = do
  CheckOptions {checkOrigin} <- asks checkOptions
  navigateTo (URI.render checkOrigin)

beforeRun :: (MonadIO m, WebDriver m, Specification spec) => spec -> Runner m ()
beforeRun spec = do
  navigateToOrigin
  do
    res <- awaitElement defaultTimeout (readyWhen spec)
    case res of
      ActionFailed s -> fail $ Text.unpack s
      _ -> pass

takeWhileChanging :: Functor m => (a -> a -> Bool) -> Pipe a a m ()
takeWhileChanging compare' = Pipes.await >>= loop
  where
    loop prev = do
      Pipes.yield prev
      next <- Pipes.await
      if next `compare'` prev then pass else loop next

takeScreenshot' :: WebDriver m => Runner m (Maybe ByteString)
takeScreenshot' = do
  CheckEnv {checkOptions = CheckOptions {checkCaptureScreenshots}} <- ask
  if checkCaptureScreenshots then Just <$> takeScreenshot else pure Nothing

observeManyStatesAfter :: (MonadIO m, MonadCatch m, WebDriver m, Selectable s, Show s) => Queries -> ActionSequence s ActionSubject -> Pipe a (TraceElement ()) (Runner m) ()
observeManyStatesAfter queries' actionSequence = do
  CheckEnv {checkScripts = scripts, checkOptions = CheckOptions {checkMaxTrailingStateChanges, checkTrailingStateChangeTimeout}} <- lift ask
  lift (runCheckScript (registerNextStateObserver scripts checkTrailingStateChangeTimeout queries'))
  (actionSequence', result) <- lift (runActionSequence actionSequence)
  lift (runCheckScript (awaitNextState scripts) `catch` (\WebDriverResponseError {} -> pass))
  newState <- lift (runCheckScript (observeState scripts queries'))
  screenshot <- lift takeScreenshot'
  Pipes.yield (TraceAction () actionSequence' result)
  Pipes.yield (TraceState () (ObservedState screenshot newState))
  nonStutters <-
    ( loop checkTrailingStateChangeTimeout
        >-> takeWhileChanging (\a b -> elementStates a == elementStates b)
        >-> Pipes.take checkMaxTrailingStateChanges
      )
      & Pipes.toListM
      & lift
  mapM_ (Pipes.yield . TraceState ()) nonStutters
  where
    loop :: (MonadCatch m, WebDriver m) => Timeout -> Producer ObservedState (Runner m) ()
    loop timeout = do
      scripts <- lift (asks checkScripts)
      newState <- lift do
        runCheckScript (registerNextStateObserver scripts timeout queries')
        runCheckScript (awaitNextState scripts) `catch` (\WebDriverResponseError {} -> pass)
        runCheckScript (observeState scripts queries')
      screenshot <- lift takeScreenshot'
      Pipes.yield (ObservedState screenshot newState)
      loop (mapTimeout (* 2) timeout)

{-# SCC runActions' "runActions'" #-}
runActions' :: (MonadIO m, MonadCatch m, WebDriver m, Selectable s, Show s, Specification spec) => spec -> Pipe (ActionSequence s ActionSubject) (TraceElement ()) (Runner m) ()
runActions' spec = do
  scripts <- lift (asks checkScripts)
  state1 <- lift (runCheckScript (observeState scripts queries'))
  screenshot <- lift takeScreenshot'
  Pipes.yield (TraceState () (ObservedState screenshot state1))
  loop
  where
    queries' = queries spec
    loop = do
      actionSequence <- Pipes.await
      observeManyStatesAfter queries' actionSequence
      loop