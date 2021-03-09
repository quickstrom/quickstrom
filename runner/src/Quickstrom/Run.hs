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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Quickstrom.Run
  ( WebDriver (..),
    WebDriverResponseError (..),
    WebDriverOtherError (..),
    CheckEnv (..),
    CheckOptions (..),
    CheckResult (..),
    PassedTest (..),
    FailedTest (..),
    Size (..),
    CheckEvent (..),
    TestEvent (..),
    check,
  )
where

import Control.Lens hiding (each)
import Control.Monad (fail, forever, when)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Natural (type (~>))
import Data.Function ((&))
import Data.Generics.Product (field)
import Data.List hiding (map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Tree
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Pipes (Pipe, Producer, (>->))
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import Quickstrom.Action
import Quickstrom.Element (Element)
import Quickstrom.Prelude hiding (catch, check, trace)
import Quickstrom.Result
import Quickstrom.Run.Actions (awaitElement, defaultTimeout, generateValidActions, reselect, runActionSequence, shrinkAction, isCurrentlyValid)
import Quickstrom.Run.Runner (CheckEnv (..), CheckOptions (..), Runner, Size (..), run)
import Quickstrom.Run.Scripts (CheckScripts (..), readScripts, runCheckScript)
import Quickstrom.Specification
import Quickstrom.Timeout (Timeout, mapTimeout)
import Quickstrom.Trace
import Quickstrom.WebDriver.Class
import qualified Test.QuickCheck as QuickCheck
import qualified Text.URI as URI

data PassedTest = PassedTest
  { trace :: Trace TraceElementEffect
  }
  deriving (Show, Generic)

data FailedTest = FailedTest
  { numShrinks :: Int,
    trace :: Trace TraceElementEffect,
    reason :: Maybe Text
  }
  deriving (Show, Generic)

data CheckResult
  = CheckSuccess {passedTests :: Vector PassedTest}
  | CheckFailure {failedAfter :: Int, passedTests :: Vector PassedTest, failedTest :: FailedTest}
  | CheckError {checkError :: Text}
  deriving (Show, Generic)

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

check ::
  (Specification spec, MonadIO n, MonadCatch n, WebDriver m, MonadIO m) =>
  CheckOptions ->
  (m ~> n) ->
  spec ->
  Pipes.Producer CheckEvent n CheckResult
check opts@CheckOptions {checkTests} runWebDriver spec = do
  -- stdGen <- getStdGen
  Pipes.yield (CheckStarted checkTests)
  env <- CheckEnv opts <$> lift readScripts
  res <-
    Pipes.hoist (runWebDriver . run env) (runAll opts spec)
      & (`catch` \err@SomeException {} -> pure (CheckError (show err)))
  Pipes.yield (CheckFinished res)
  pure res

elementsToTrace :: Monad m => Producer (TraceElement ()) (Runner m) () -> Runner m (Trace ())
elementsToTrace = fmap Trace . Pipes.toListM

minBy :: (Monad m, Ord b) => (a -> b) -> Producer a m () -> m (Maybe a)
minBy f = Pipes.fold step Nothing identity
  where
    step x a = Just $ case x of
      Nothing -> a
      Just a' ->
        case f a `compare` f a' of
          EQ -> a
          LT -> a
          GT -> a'

select :: Monad m => (a -> Maybe b) -> Pipe a b m ()
select f = forever do
  x <- Pipes.await
  maybe (pure ()) Pipes.yield (f x)

runSingle ::
  (MonadIO m, WebDriver m, Specification spec) =>
  spec ->
  Size ->
  Producer TestEvent (Runner m) (Either FailedTest PassedTest)
runSingle spec size = do
  Pipes.yield (TestStarted size)
  result <-
    generateValidActions (actions spec)
      >-> Pipes.take (fromIntegral (unSize size))
      & runAndVerifyIsolated 0
  case result of
    Right trace -> do
      Pipes.yield (TestPassed size trace)
      pure (Right (PassedTest trace))
    Left ft@(FailedTest _ trace _) -> do
      CheckOptions {checkShrinkLevels} <- lift (asks checkOptions)
      Pipes.yield (TestFailed size trace)
      if checkShrinkLevels > 0
        then do
          Pipes.yield (Shrinking checkShrinkLevels)
          let shrinks = shrinkForest (QuickCheck.shrinkList shrinkAction) checkShrinkLevels (trace ^.. traceActions)
          shrunk <-
            minBy
              (lengthOf (field @"trace" . traceElements))
              ( traverseShrinks runShrink shrinks
                  >-> select (preview _Left)
              )
          pure (maybe (Left ft) Left shrunk)
        else pure (Left ft)
  where
    runAndVerifyIsolated ::
      (MonadIO m, WebDriver m) =>
      Int ->
      Producer (ActionSequence (Element, Selected)) (Runner m) () ->
      Producer TestEvent (Runner m) (Either FailedTest (Trace TraceElementEffect))
    runAndVerifyIsolated n producer = do
      trace <- lift do
        opts <- asks checkOptions
        annotateStutteringSteps <$> inNewPrivateWindow (checkWebDriverOptions opts) do
          beforeRun spec
          elementsToTrace (producer >-> runActions' spec)
      case verify spec (trace ^.. nonStutterStates) of
        Right Accepted -> pure (Right trace)
        Right Rejected -> pure (Left (FailedTest n trace Nothing))
        Left err -> pure (Left (FailedTest n trace (Just err)))
    runShrink ::
      (MonadIO m, WebDriver m) =>
      Shrink [ActionSequence Selected] ->
      Producer TestEvent (Runner m) (Either FailedTest (Trace TraceElementEffect))
    runShrink (Shrink n actions') = do
      Pipes.yield (RunningShrink n)
      Pipes.each actions'
        >-> Pipes.mapM (traverse reselect)
        >-> Pipes.mapMaybe (traverse identity)
        >-> Pipes.filterM isCurrentlyValid
        & runAndVerifyIsolated n 

runAll :: (MonadIO m, WebDriver m, Specification spec) => CheckOptions -> spec -> Producer CheckEvent (Runner m) CheckResult
runAll opts spec' = go mempty (sizes opts `zip` [1 ..])
  where
    go :: (MonadIO m, WebDriver m) => Vector PassedTest -> [(Size, Int)] -> Producer CheckEvent (Runner m) CheckResult
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

observeManyStatesAfter :: (MonadIO m, WebDriver m) => Queries -> ActionSequence (Element, Selected) -> Pipe a (TraceElement ()) (Runner m) ()
observeManyStatesAfter queries' actionSequence = do
  CheckEnv {checkScripts = scripts, checkOptions = CheckOptions {checkMaxTrailingStateChanges, checkTrailingStateChangeTimeout}} <- lift ask
  lift (runCheckScript (registerNextStateObserver scripts checkTrailingStateChangeTimeout queries'))
  result <- lift (runActionSequence (map fst actionSequence))
  lift (runCheckScript (awaitNextState scripts) `catchResponseError` const pass)
  newState <- lift (runCheckScript (observeState scripts queries'))
  screenshot <- lift takeScreenshot'
  Pipes.yield (TraceAction () (map snd actionSequence) result)
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
    loop :: WebDriver m => Timeout -> Producer ObservedState (Runner m) ()
    loop timeout = do
      scripts <- lift (asks checkScripts)
      newState <- lift do
        runCheckScript (registerNextStateObserver scripts timeout queries')
        runCheckScript (awaitNextState scripts) `catchResponseError` const pass
        runCheckScript (observeState scripts queries')
      screenshot <- lift takeScreenshot'
      Pipes.yield (ObservedState screenshot newState)
      loop (mapTimeout (* 2) timeout)

{-# SCC runActions' "runActions'" #-}
runActions' :: (MonadIO m, WebDriver m, Specification spec) => spec -> Pipe (ActionSequence (Element, Selected)) (TraceElement ()) (Runner m) ()
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

data Shrink a = Shrink Int a

shrinkForest :: (a -> [a]) -> Int -> a -> Forest (Shrink a)
shrinkForest shrink limit = go 1
  where
    go n
      | n <= limit = map (\x -> Node (Shrink n x) (go (succ n) x)) . shrink
      | otherwise = mempty

traverseShrinks :: Monad m => (Shrink a -> m (Either e b)) -> Forest (Shrink a) -> Producer (Either e b) m ()
traverseShrinks test = go
  where
    go = \case
      [] -> pure ()
      Node x xs : rest -> do
        r <- lift (test x)
        Pipes.yield r
        when (isn't _Right r) do
          go xs
        go rest
