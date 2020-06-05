{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module WebCheck.Run
  ( testSpecifications,
  )
where

import Control.Lens hiding (each)
import Control.Monad ((>=>), forever, void, when)
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Loops (andM)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Identity (IdentityT)
import qualified Data.Aeson as JSON
import Data.Function ((&))
import Data.Functor (($>))
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Tree
import GHC.Generics (Generic)
import Network.HTTP.Client as Http
import qualified Network.Wreq as Wreq
import Pipes ((>->), Consumer, Effect, Pipe, Producer)
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import qualified Test.QuickCheck as QuickCheck
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (assertFailure, testCase)
import WebCheck.Element
import WebCheck.Formula
import WebCheck.Query
import WebCheck.Result
import WebCheck.Specification
import WebCheck.Trace
import WebCheck.Verify
import Web.Api.WebDriver hiding (Action, Selector, assertFailure, hPutStrLn, runIsolated)

type Runner = WebDriverTT IdentityT IO

testSpecifications :: [(Text, Specification Formula)] -> Tasty.TestTree
testSpecifications specs =
  Tasty.testGroup "WebCheck specifications" [testCase (Text.unpack name) (check spec) | (name, spec) <- specs]

data FailingTest = FailingTest {numShrinks :: Int, trace :: Trace TraceElementEffect, reason :: Maybe EvalError}
  deriving (Show, Generic)

data CheckResult = CheckSuccess | CheckFailure {failedAfter :: Int, failingTest :: FailingTest}
  deriving (Show)

check :: Specification Formula -> IO ()
check spec = do
  -- stdGen <- getStdGen
  let numTests = 10
  logInfo ("Running " <> show numTests <> " tests...")
  result <- runWebDriver (Pipes.runEffect (runAll numTests spec'))
  case result of
    CheckFailure {failedAfter, failingTest} -> do
      logInfo . renderString $
        prettyTrace (withoutStutterStates (trace failingTest)) <> line
      case reason failingTest of
        Just err -> logInfo (renderString (annotate (color Red) ("Verification failed with error:" <+> prettyEvalError err <> line)))
        Nothing -> pure ()
      assertFailure ("Failed after " <> show failedAfter <> " tests and " <> show (numShrinks failingTest) <> " levels of shrinking.")
    CheckSuccess -> logInfo ("Passed " <> show numTests <> " tests.")
  where
    spec' = spec & field @"proposition" %~ simplify

prettyEvalError :: EvalError -> Doc AnsiStyle
prettyEvalError = \case
  TypeError value type' -> prettyValue value <+> "is not a" <+> pretty type'
  ApplyError f args -> prettyValue f <+> "cannot be applied to" <+> hsep (punctuate comma (map prettyValue args))
  QueryError query -> "Query failed: " <+> prettyQuery query
  RuntimeError t -> "Verification failed with error: " <+> pretty t
  Undetermined -> "Verification failed with undetermination. There are too few observed states to satisfy the specification."

elementsToTrace :: Producer (TraceElement ()) Runner () -> Runner (Trace ())
elementsToTrace = fmap Trace . Pipes.toListM

minBy :: (Monad m, Ord b) => (a -> b) -> Producer a m () -> m (Maybe a)
minBy f = Pipes.fold step Nothing id
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

runSingle :: Specification Formula -> Int -> Runner (Either FailingTest ())
runSingle spec size = do
  let maxTries = size * 10
  result <-
    generateActions (actions spec)
      >-> Pipes.take maxTries
      >-> selectValidActions
      >-> Pipes.take size
      & runAndVerifyIsolated 0
  case result of
    Right () -> pure (Right ())
    f@(Left (FailingTest _ trace _)) -> do
      logInfoWD "Test failed. Shrinking..."
      let shrinks = shrinkForest (QuickCheck.shrinkList shrinkAction) (trace ^.. traceActions)
      shrunk <- minBy (lengthOf (field @"trace" . traceElements)) (traverseShrinks runShrink shrinks >-> select (preview _Left) >-> Pipes.take 10)
      pure (fromMaybe f (Left <$> shrunk))
  where
    runAndVerifyIsolated n producer = do
      trace <- annotateStutteringSteps <$> inNewPrivateWindow do
        beforeRun spec
        elementsToTrace (producer >-> runActions' spec)
      case verify (trace ^.. nonStutterStates) (proposition spec) of
        Right Accepted -> pure (Right ())
        Right Rejected -> pure (Left (FailingTest n trace Nothing))
        Left err -> pure (Left (FailingTest n trace (Just err)))
    runShrink (Shrink n actions) =
      runAndVerifyIsolated n (Pipes.each actions)

runAll :: Int -> Specification Formula -> Effect Runner CheckResult
runAll numTests spec' = (allTests $> CheckSuccess) >-> firstFailure
  where
    runSingle' :: Int -> Producer (Either FailingTest ()) Runner ()
    runSingle' size = do
      lift (logInfoWD ("Running test with size: " <> show size))
      Pipes.yield =<< lift (runSingle spec' size)
    allTests :: Producer (Either FailingTest (), Int) Runner ()
    allTests = Pipes.for (sizes numTests) runSingle' `Pipes.zip` Pipes.each [1 ..]

firstFailure :: Functor m => Consumer (Either FailingTest (), Int) m CheckResult
firstFailure =
  Pipes.await >>= \case
    (Right {}, _) -> firstFailure
    (Left failingTest, n) -> pure (CheckFailure n failingTest)

sizes :: Functor m => Int -> Producer Int m ()
sizes numSizes = Pipes.each (map (\n -> (n * 100 `div` numSizes)) [1 .. numSizes])

beforeRun :: Specification Formula -> Runner ()
beforeRun spec = do
  navigateToOrigin spec
  initializeScript
  awaitElement (readyWhen spec)

observeManyStatesAfter :: HashSet Query -> ObservedState -> Action Selected -> Pipe a (TraceElement ()) Runner ObservedState
observeManyStatesAfter queries initialState action = do
  result <- lift (runAction action)
  delta <- getNextOrFail =<< lift (registerNextStateObserver queries)
  Pipes.yield (TraceAction () action result)
  nonStutters <-
    (loop (delta <> initialState) >-> Pipes.takeWhile (/= initialState) >-> Pipes.take 10)
      & Pipes.toListM
      & lift
      & fmap (fromMaybe (pure initialState) . NonEmpty.nonEmpty)
  mapM_ (Pipes.yield . (TraceState ())) nonStutters
  pure (NonEmpty.last nonStutters)
  where
    getNextOrFail observer =
      either (fail . Text.unpack) pure
        =<< lift (getNextState observer)
    loop :: ObservedState -> Producer ObservedState Runner ()
    loop currentState = do
      Pipes.yield currentState
      delta <- getNextOrFail =<< lift (registerNextStateObserver queries)
      loop (delta <> currentState)

{-# SCC runActions' "runActions'" #-}
runActions' :: Specification Formula -> Pipe (Action Selected) (TraceElement ()) Runner ()
runActions' spec = do
  state1 <- lift (observeStates queries)
  Pipes.yield (TraceState () state1)
  loop state1
  where
    queries = HashSet.fromList (runIdentity (withQueries pure (proposition spec)))
    loop currentState = do
      action <- Pipes.await
      newState <- observeManyStatesAfter queries currentState action
      loop newState

data Shrink a = Shrink Int a

shrinkForest :: (a -> [a]) -> a -> Forest (Shrink a)
shrinkForest shrink = go 1
  where
    go n = map (\x -> Node (Shrink n x) (go (succ n) x)) . shrink

traverseShrinks :: Monad m => ((Shrink a) -> m (Either e ())) -> Forest (Shrink a) -> Producer (Either e ()) m ()
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

generate :: QuickCheck.Gen a -> Runner a
generate = liftWebDriverTT . lift . QuickCheck.generate

generateActions :: ActionGenerator -> Producer (Action Selector) Runner ()
generateActions gen = forever do
  Pipes.yield =<< lift (generate gen)

selectValidActions :: Pipe (Action Selector) (Action Selected) Runner ()
selectValidActions = forever do
  possibleAction <- Pipes.await
  result <- case possibleAction of
    KeyPress k ->
      lift $
        activeElement >>= \case
          Just el ->
            getElementTagName el >>= \case
              name
                | name `elem` ["input", "textarea"] -> pure (Just (KeyPress k))
                | otherwise -> pure Nothing
          Nothing -> pure Nothing
    Navigate p -> pure (Just (Navigate p))
    Focus sel -> lift (selectOne sel Focus (isNotActive . toRef))
    Click sel -> lift (selectOne sel Click isClickable)
  maybe mempty Pipes.yield result
  where
    selectOne :: Selector -> (Selected -> Action Selected) -> (Element -> Runner Bool) -> Runner (Maybe (Action Selected))
    selectOne sel ctor isValid = do
      found <- findAll sel
      validChoices <-
        ( filterM
            (\(_, e) -> isValid e `catchError` (const (pure False)))
            (zip [0 ..] found)
          )
      case validChoices of
        [] -> pure Nothing
        choices -> Just <$> generate (ctor . Selected sel <$> QuickCheck.elements (map fst choices))
    isNotActive e = (/= Just e) <$> activeElement
    activeElement = (Just <$> getActiveElement) `catchError` const (pure Nothing)
    isClickable e =
      andM [isElementEnabled (toRef e), isElementVisible e]

navigateToOrigin :: Specification formula -> Runner ()
navigateToOrigin spec = case origin spec of
  Path path -> (navigateTo (Text.unpack path))

tryAction :: Runner ActionResult -> Runner ActionResult
tryAction action = action `catchError` (pure . ActionFailed . Text.pack . show)

click :: Selected -> Runner ActionResult
click = findSelected >=> \case
  Just e -> tryAction (ActionSuccess <$ (elementClick (toRef e)))
  Nothing -> pure ActionImpossible

sendKey :: Char -> Runner ActionResult
sendKey c = tryAction (ActionSuccess <$ (getActiveElement >>= elementSendKeys [c]))

focus :: Selected -> Runner ActionResult
focus = findSelected >=> \case
  Just e -> tryAction (ActionSuccess <$ (elementSendKeys "" (toRef e)))
  Nothing -> pure ActionImpossible

runAction :: Action Selected -> Runner ActionResult
runAction = \case
  Focus s -> focus s
  KeyPress c -> sendKey c
  Click s -> click s
  Navigate (Path path) -> tryAction (ActionSuccess <$ navigateTo (Text.unpack path))

runWebDriver :: Runner a -> IO a
runWebDriver ma = do
  mgr <- Http.newManager defaultManagerSettings
  let httpOptions :: Wreq.Options
      httpOptions = Wreq.defaults & Wreq.manager .~ Right mgr
  execWebDriverT (reconfigure defaultWebDriverConfig httpOptions) ma >>= \case
    (Right x, _, _) -> pure x
    (Left err, _, _) -> fail (show err)
  where
    reconfigure c httpOptions =
      c
        { _environment =
            (_environment c)
              { _logEntryPrinter = \_ _ -> Nothing
              },
          _initialState = defaultWebDriverState {_httpOptions = httpOptions}
        }

inNewPrivateWindow :: Runner a -> Runner a
inNewPrivateWindow = runIsolated (reconfigure headlessFirefoxCapabilities)
  where
    reconfigure c =
      c
        { _firefoxOptions = (_firefoxOptions c)
            <&> \o ->
              o
                { _firefoxArgs = Just ["-headless", "-private"],
                  _firefoxPrefs = Just (HashMap.singleton "Dom.storage.enabled" (JSON.Bool False)),
                  _firefoxLog = Just (FirefoxLog (Just LogWarn))
                }
        }

-- | Mostly the same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
runIsolated ::
  (Monad eff, Monad (t eff), MonadTrans t) =>
  Capabilities ->
  WebDriverTT t eff a ->
  WebDriverTT t eff a
runIsolated caps theSession = cleanupOnError do
  sid <- newSession caps
  modifyState (setSessionId (Just sid))
  a <- theSession
  deleteSession
  modifyState (setSessionId Nothing)
  pure a

-- | Same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
cleanupOnError ::
  (Monad eff, Monad (t eff), MonadTrans t) =>
  -- | `WebDriver` session that may throw errors
  WebDriverTT t eff a ->
  WebDriverTT t eff a
cleanupOnError x =
  catchAnyError
    x
    (\e -> deleteSession >> throwError e)
    (\e -> deleteSession >> throwHttpException e)
    (\e -> deleteSession >> throwIOException e)
    (\e -> deleteSession >> throwJsonError e)

-- | Same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
setSessionId ::
  Maybe String ->
  S WDState ->
  S WDState
setSessionId x st = st {_userState = (_userState st) {_sessionId = x}}

findSelected :: Selected -> Runner (Maybe Element)
findSelected (Selected s i) =
  findAll s >>= \case
    es
      | length es > i -> pure (Just (es !! i))
      | otherwise -> pure Nothing

findAll :: Selector -> Runner [Element]
findAll (Selector s) = map fromRef <$> findElements CssSelector (Text.unpack s)

toRef :: Element -> ElementRef
toRef (Element ref) = ElementRef (Text.unpack ref)

fromRef :: ElementRef -> Element
fromRef (ElementRef ref) = Element (Text.pack ref)

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn

logInfoWD :: String -> Runner ()
logInfoWD = liftWebDriverTT . logInfo

isElementVisible :: Element -> Runner Bool
isElementVisible el =
  (== JSON.Bool True) <$> executeScript "return window.wtp.isElementVisible(arguments[0])" [JSON.toJSON el]

awaitElement :: Selector -> WebDriverT IO ()
awaitElement (Selector sel) =
  void (executeAsyncScript "window.wtp.awaitElement(arguments[0], arguments[1])" [JSON.toJSON sel])

executeScript' :: JSON.FromJSON r => Script -> [JSON.Value] -> Runner r
executeScript' script args = do
  r <- executeScript script args
  case JSON.fromJSON r of
    JSON.Success a -> pure a
    JSON.Error e -> fail e

executeAsyncScript' :: JSON.FromJSON r => Script -> [JSON.Value] -> Runner r
executeAsyncScript' script args = do
  r <- executeAsyncScript script args
  case JSON.fromJSON r of
    JSON.Success a -> pure a
    JSON.Error e -> fail e

observeStates :: HashSet Query -> WebDriverT IO ObservedState
observeStates queries =
  executeScript' "return wtp.mapToArray(window.wtp.observeInitialStates(arguments[0]))" [JSON.toJSON queries]

newtype StateObserver = StateObserver Text
  deriving (Eq, Show)

registerNextStateObserver :: HashSet Query -> Runner StateObserver
registerNextStateObserver queries =
  StateObserver
    <$> executeScript'
      "return wtp.registerNextStateObserver(arguments[0])"
      [JSON.toJSON queries]

getNextState :: StateObserver -> Runner (Either Text ObservedState)
getNextState (StateObserver sid) =
  executeAsyncScript'
    "wtp.runPromiseEither(wtp.getNextState(arguments[0]).then(wtp.mapToArray), arguments[1])"
    [JSON.toJSON sid]

renderString :: Doc AnsiStyle -> String
renderString = Text.unpack . renderStrict . layoutPretty defaultLayoutOptions

wtpJs :: Runner Script
wtpJs = liftWebDriverTT (lift (readFile "target/index.js"))

initializeScript :: Runner ()
initializeScript = do
  js <- wtpJs
  void (executeScript js [])
