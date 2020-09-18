{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Quickstrom.Run
  ( WebDriver (..),
    WebDriverResponseError (..),
    WebDriverOtherError (..),
    CheckEnv (..),
    CheckResult (..),
    FailingTest (..),
    CheckEvent (..),
    TestEvent (..),
    check,
  )
where

import Control.Lens hiding (each)
import Control.Monad (fail, filterM, forever, void, when, (>=>))
import Control.Monad.Catch (MonadCatch, MonadThrow, catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Loops (andM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Natural (type (~>))
import qualified Data.Aeson as JSON
import Data.Function ((&))
import qualified Data.Generic.HKD as HKD
import Data.Generics.Product (field)
import Data.Maybe (fromMaybe)
import Data.String (String, fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Tree
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Pipes (Pipe, Producer, (>->))
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import Quickstrom.Element
import Quickstrom.Options
import Quickstrom.Prelude hiding (catch, check, trace)
import Quickstrom.Result
import Quickstrom.Specification
import Quickstrom.Trace
import Quickstrom.WebDriver.Class
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified Test.QuickCheck as QuickCheck
import qualified Text.URI as URI

newtype Runner m a = Runner (ReaderT CheckEnv m a)
  deriving (Functor, Applicative, Monad, MonadIO, WebDriver, MonadReader CheckEnv, MonadThrow, MonadCatch)

run :: CheckEnv -> Runner m a -> m a
run env (Runner ma) = runReaderT ma env

data FailingTest = FailingTest
  { numShrinks :: Int,
    trace :: Trace TraceElementEffect,
    reason :: Maybe Text
  }
  deriving (Show, Generic)

instance JSON.ToJSON FailingTest where
  toJSON = JSON.genericToJSON JSON.defaultOptions

data CheckResult
  = CheckSuccess
  | CheckFailure {failedAfter :: Int, failingTest :: FailingTest}
  | CheckError {checkError :: Text}
  deriving (Show, Generic)

instance JSON.ToJSON CheckResult where
  toJSON = JSON.genericToJSON JSON.defaultOptions

data TestEvent
  = TestStarted Size
  | TestPassed Size (Trace TraceElementEffect)
  | TestFailed Size (Trace TraceElementEffect)
  | Shrinking Int
  | RunningShrink Int
  deriving (Show, Generic)

instance JSON.ToJSON TestEvent where
  toJSON = JSON.genericToJSON JSON.defaultOptions

data CheckEvent
  = CheckStarted Int
  | CheckTestEvent TestEvent
  | CheckFinished CheckResult
  deriving (Show, Generic)

instance JSON.ToJSON CheckEvent where
  toJSON = JSON.genericToJSON JSON.defaultOptions

data CheckEnv = CheckEnv {checkOptions :: CheckOptionsFirst, checkScripts :: CheckScripts}

mapTimeout :: (Word64 -> Word64) -> Timeout -> Timeout
mapTimeout f (Timeout ms) = Timeout (f ms)

data CheckScript a = CheckScript {runCheckScript :: forall m. WebDriver m => m a}

data CheckScripts = CheckScripts
  { isElementVisible :: Element -> CheckScript Bool,
    observeState :: Queries -> CheckScript ObservedState,
    registerNextStateObserver :: Timeout -> Queries -> CheckScript (),
    awaitNextState :: CheckScript (Maybe ObservedState)
  }

check ::
  (Specification spec, MonadIO n, MonadCatch n, WebDriver m, MonadIO m) =>
  CheckOptionsFirst ->
  (m ~> n) ->
  spec ->
  Pipes.Producer CheckEvent n CheckResult
check opts runWebDriver spec = do
  -- stdGen <- getStdGen
  let defaultCheckTests = defaultOptsF ^. HKD.field @"checkTests"
      cliCheckTests = opts ^. HKD.field @"checkTests"
  Pipes.yield (CheckStarted $ selectOption defaultCheckTests [cliCheckTests])
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

runSingle :: (MonadIO m, WebDriver m, Specification spec) => spec -> Size -> Producer TestEvent (Runner m) (Either FailingTest ())
runSingle spec size = do
  Pipes.yield (TestStarted size)
  result <-
    generateValidActions (actions spec)
      >-> Pipes.take (fromIntegral (unSize size))
      & runAndVerifyIsolated 0
  case result of
    Right trace -> do
      Pipes.yield (TestPassed size trace)
      pure (Right ())
    f@(Left (FailingTest _ trace _)) -> do
      opts <- lift (asks checkOptions)
      Pipes.yield (TestFailed size trace)
      let defaultCheckShrinkLevels = defaultOptsF ^. HKD.field @"checkShrinkLevels"
          cliCheckShrinkLevels = opts ^. HKD.field @"checkShrinkLevels"
          shrinkLevel = selectOption defaultCheckShrinkLevels [cliCheckShrinkLevels]
      if (shrinkLevel > 0)
        then do
          Pipes.yield (Shrinking shrinkLevel)
          let shrinks = shrinkForest (QuickCheck.shrinkList shrinkAction) shrinkLevel (trace ^.. traceActions)
          shrunk <-
            minBy
              (lengthOf (field @"trace" . traceElements))
              ( traverseShrinks runShrink shrinks
                  >-> select (preview _Left)
                  >-> Pipes.take 5
              )
          pure (fromMaybe (void f) (Left <$> shrunk))
        else pure (void f)
  where
    runAndVerifyIsolated ::
      (MonadIO m, WebDriver m) =>
      Int ->
      Producer (Action Selected) (Runner m) () ->
      Producer TestEvent (Runner m) (Either FailingTest (Trace TraceElementEffect))
    runAndVerifyIsolated n producer = do
      trace <- lift do
        opts <- asks checkOptions
        let defaultDriverOptions = defaultOptsF ^. HKD.field @"checkWebDriverOptions"
            cliDriverOptions = opts ^. HKD.field @"checkWebDriverOptions"
            driverOption = selectOption defaultDriverOptions [cliDriverOptions]
        annotateStutteringSteps <$> inNewPrivateWindow driverOption do
          beforeRun spec
          elementsToTrace (producer >-> runActions' spec)
      case verify spec (trace ^.. nonStutterStates) of
        Right Accepted -> pure (Right trace)
        Right Rejected -> pure (Left (FailingTest n trace Nothing))
        Left err -> pure (Left (FailingTest n trace (Just err)))
    runShrink (Shrink n actions') = do
      Pipes.yield (RunningShrink n)
      runAndVerifyIsolated n (Pipes.each actions')

runAll :: (MonadIO m, WebDriver m, Specification spec) => CheckOptionsFirst -> spec -> Producer CheckEvent (Runner m) CheckResult
runAll opts spec' = go (sizes opts `zip` [1 ..])
  where
    go :: (MonadIO m, WebDriver m) => [(Size, Int)] -> Producer CheckEvent (Runner m) CheckResult
    go [] = pure CheckSuccess
    go ((size, n) : rest) =
      (runSingle spec' size >-> Pipes.map CheckTestEvent)
        >>= \case
          Right {} -> go rest
          Left failingTest -> pure (CheckFailure n failingTest)

sizes :: CheckOptionsFirst -> [Size]
sizes opts =
  let defaultMaxActions = defaultOptsF ^. HKD.field @"checkMaxActions"
      cliMaxAction = opts ^. HKD.field @"checkMaxActions"
      defaultCheckTests = defaultOptsF ^. HKD.field @"checkTests"
      cliCheckTests = opts ^. HKD.field @"checkTests"
      checktest = selectOption defaultCheckTests [cliCheckTests]
      (Size maxAction) = selectOption defaultMaxActions [cliMaxAction]
  in
    map (\n -> Size (n * maxAction `div` fromIntegral checktest)) [1 .. fromIntegral checktest]

beforeRun :: (MonadIO m, WebDriver m, Specification spec) => spec -> Runner m ()
beforeRun spec = do
  navigateToOrigin
  awaitElement (readyWhen spec)

takeWhileChanging :: (Eq a, Functor m) => Pipe a a m ()
takeWhileChanging = Pipes.await >>= loop
  where
    loop last = do
      Pipes.yield last
      next <- Pipes.await
      if next == last then pass else loop next

observeManyStatesAfter :: WebDriver m => Queries -> Action Selected -> Pipe a (TraceElement ()) (Runner m) ()
observeManyStatesAfter queries' action = do
  CheckEnv {checkScripts = scripts, checkOptions = opts} <- lift ask
  let defaultCTSCTimeout = defaultOptsF ^. HKD.field @"checkTrailingStateChangeTimeout"
      cliCTSCTimeout = opts ^. HKD.field @"checkTrailingStateChangeTimeout"
      cTSCTimeout = selectOption defaultCTSCTimeout [cliCTSCTimeout]
      defaultMaxCTSCTimeout = defaultOptsF ^. HKD.field @"checkMaxTrailingStateChanges"
      cliMaxCTSCTimeout = opts ^. HKD.field @"checkMaxTrailingStateChanges"
      cMaxTSCTimeout = selectOption defaultMaxCTSCTimeout [cliMaxCTSCTimeout]

  lift (runCheckScript (registerNextStateObserver scripts cTSCTimeout queries'))
  result <- lift (runAction action)
  newState <-
    lift (runCheckScript (awaitNextState scripts)) >>= \case
      Just state' -> pure state'
      Nothing -> lift (runCheckScript (observeState scripts queries'))
  Pipes.yield (TraceAction () action result)
  Pipes.yield (TraceState () newState)
  nonStutters <-
    (loop cTSCTimeout >-> takeWhileChanging >-> Pipes.take cMaxTSCTimeout)
      & Pipes.toListM
      & lift
  mapM_ (Pipes.yield . (TraceState ())) nonStutters
  where
    loop :: WebDriver m => Timeout -> Producer ObservedState (Runner m) ()
    loop timeout = do
      scripts <- lift (asks checkScripts)
      newState <- lift do
        runCheckScript (registerNextStateObserver scripts timeout queries')
        runCheckScript (awaitNextState scripts) >>= \case
          Just state' -> pure state'
          Nothing -> runCheckScript (observeState scripts queries')
      Pipes.yield newState
      loop (mapTimeout (* 2) timeout)

{-# SCC runActions' "runActions'" #-}
runActions' :: (WebDriver m, Specification spec) => spec -> Pipe (Action Selected) (TraceElement ()) (Runner m) ()
runActions' spec = do
  scripts <- lift (asks checkScripts)
  state1 <- lift (runCheckScript (observeState scripts queries'))
  Pipes.yield (TraceState () state1)
  loop
  where
    queries' = queries spec
    loop = do
      action <- Pipes.await
      observeManyStatesAfter queries' action
      loop

data Shrink a = Shrink Int a

shrinkForest :: (a -> [a]) -> Int -> a -> Forest (Shrink a)
shrinkForest shrink limit = go 1
  where
    go n
      | n <= limit = map (\x -> Node (Shrink n x) (go (succ n) x)) . shrink
      | otherwise = mempty

traverseShrinks :: Monad m => ((Shrink a) -> m (Either e b)) -> Forest (Shrink a) -> Producer (Either e b) m ()
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

shrinkAction :: Action sel -> [Action sel]
shrinkAction _ = [] -- TODO?

generate :: MonadIO m => QuickCheck.Gen a -> m a
generate = liftIO . QuickCheck.generate

generateValidActions :: (MonadIO m, WebDriver m) => Vector (Int, Action Selector) -> Producer (Action Selected) (Runner m) ()
generateValidActions possibleActions = loop
  where
    loop = do
      validActions <- lift $ for (Vector.toList possibleActions) \(prob, action') -> do
        fmap (prob,) <$> selectValidAction action'
      case map (_2 %~ pure) (catMaybes validActions) of
        [] -> pass
        actions' -> do
          actions'
            & QuickCheck.frequency
            & generate
            & lift
            & (>>= Pipes.yield)
          loop

selectValidAction :: (MonadIO m, WebDriver m) => Action Selector -> Runner m (Maybe (Action Selected))
selectValidAction possibleAction =
  case possibleAction of
    KeyPress k -> do
      active <- isActiveInput
      if active then (pure (Just (KeyPress k))) else pure Nothing
    EnterText t -> do
      active <- isActiveInput
      if active then (pure (Just (EnterText t))) else pure Nothing
    Navigate p -> pure (Just (Navigate p))
    Focus sel -> selectOne sel Focus isNotActive
    Click sel -> selectOne sel Click isClickable
  where
    selectOne ::
      (MonadIO m, WebDriver m) =>
      Selector ->
      (Selected -> Action Selected) ->
      (Element -> (Runner m) Bool) ->
      Runner m (Maybe (Action Selected))
    selectOne sel ctor isValid = do
      found <- findAll sel
      validChoices <-
        ( filterM
            (\(_, e) -> isValid e `catchResponseError` const (pure False))
            (zip [0 ..] found)
          )
      case validChoices of
        [] -> pure Nothing
        choices -> Just <$> generate (ctor . Selected sel <$> QuickCheck.elements (map fst choices))
    isNotActive e = (/= Just e) <$> activeElement
    activeElement = (Just <$> getActiveElement) `catchResponseError` const (pure Nothing)
    isClickable e = do
      scripts <- asks checkScripts
      andM [isElementEnabled e, runCheckScript (isElementVisible scripts e)]
    isActiveInput =
      activeElement >>= \case
        Just el -> (`elem` ["input", "textarea"]) <$> getElementTagName el
        Nothing -> pure False

navigateToOrigin :: WebDriver m => Runner m ()
navigateToOrigin = do
  opts <- asks checkOptions
  let defaultCheckOrigin = defaultOptsF ^. HKD.field @"checkOrigin"
      cliCheckOrigin = opts ^. HKD.field @"checkOrigin"
      checkOrigin = selectOption defaultCheckOrigin [cliCheckOrigin]
  navigateTo (URI.render checkOrigin)

tryAction :: WebDriver m => Runner m ActionResult -> Runner m ActionResult
tryAction action =
  action
    `catchResponseError` (\(WebDriverResponseError msg) -> pure (ActionFailed msg))

click :: WebDriver m => Selected -> Runner m ActionResult
click =
  findSelected >=> \case
    Just e -> tryAction (ActionSuccess <$ (elementClick e))
    Nothing -> pure ActionImpossible

sendKeys :: WebDriver m => Text -> Runner m ActionResult
sendKeys t = tryAction (ActionSuccess <$ (getActiveElement >>= elementSendKeys t))

sendKey :: WebDriver m => Char -> Runner m ActionResult
sendKey = sendKeys . Text.singleton

focus :: WebDriver m => Selected -> Runner m ActionResult
focus =
  findSelected >=> \case
    Just e -> tryAction (ActionSuccess <$ (elementSendKeys "" e))
    Nothing -> pure ActionImpossible

runAction :: WebDriver m => Action Selected -> Runner m ActionResult
runAction = \case
  Focus s -> focus s
  KeyPress c -> sendKey c
  EnterText t -> sendKeys t
  Click s -> click s
  Navigate uri -> tryAction (ActionSuccess <$ navigateTo uri)

findSelected :: WebDriver m => Selected -> Runner m (Maybe Element)
findSelected (Selected s i) =
  findAll s >>= \es -> pure ((es ^? ix i))

awaitElement :: (MonadIO m, WebDriver m) => Selector -> Runner m ()
awaitElement sel@(Selector s) =
  let loop n
        | n > 10 = fail ("Giving up after having waited 10 seconds for selector to match an element: " <> toS s)
        | otherwise =
          findAll sel >>= \case
            [] -> liftIO (threadDelay 1000000) >> loop (n + 1)
            _ -> pass
   in loop (1 :: Int)

readScripts :: MonadIO m => m CheckScripts
readScripts = do
  let key = "QUICKSTROM_CLIENT_SIDE_DIR"
  dir <- liftIO (maybe (fail (key <> " environment variable not set")) pure =<< lookupEnv key)
  let readScript :: MonadIO m => String -> m Text
      readScript name = liftIO (fromString . toS <$> readFile (dir </> name <> ".js"))
  isElementVisibleScript <- readScript "isElementVisible"
  observeStateScript <- readScript "observeState"
  registerNextStateObserverScript <- readScript "registerNextStateObserver"
  awaitNextStateScript <- readScript "awaitNextState"
  pure
    CheckScripts
      { isElementVisible = \el -> CheckScript ((== JSON.Bool True) <$> runScript isElementVisibleScript [JSON.toJSON el]),
        observeState = \queries' -> CheckScript (runScript observeStateScript [JSON.toJSON queries']),
        registerNextStateObserver = \timeout queries' -> CheckScript (runScript registerNextStateObserverScript [JSON.toJSON timeout, JSON.toJSON queries']),
        awaitNextState = CheckScript (runScript awaitNextStateScript [])
      }
