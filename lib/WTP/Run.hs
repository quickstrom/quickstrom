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

module WTP.Run
  ( testSpecifications,
  )
where

import Control.Applicative ((<|>))
import Control.Lens hiding (each)
import Control.Monad ((>=>), void)
import Control.Monad (filterM)
import Control.Monad.Freer (Eff, Member, reinterpret3, runM, sendM, type (~>))
import Control.Monad.Freer.State (State, evalState, get, put)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Identity (IdentityT)
import qualified Data.Aeson as JSON
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Typeable (cast)
import GHC.Generics (Generic)
import Network.HTTP.Client as Http
import qualified Network.Wreq as Wreq
import Pipes ((>->), (>~), Consumer, Effect, Pipe, Producer, X)
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import qualified Test.QuickCheck as QuickCheck
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (assertFailure, testCase)
import WTP.Element
import WTP.Formula
import WTP.Query
import WTP.Result
import WTP.Specification
import WTP.TH (embedStringFile')
import WTP.Trace
import WTP.Verify
import Web.Api.WebDriver hiding (Action, Selector, assertFailure, hPutStrLn, runIsolated)

type Runner = WebDriverTT IdentityT IO

testSpecifications :: [(Text, Specification Proposition)] -> Tasty.TestTree
testSpecifications specs =
  Tasty.testGroup "WTP specifications" [testCase (Text.unpack name) (check spec) | (name, spec) <- specs]

data FailingTest = FailingTest {numShrinks :: Int, trace :: Trace TraceElementEffect}
  deriving (Show)

type TestResult = Either FailingTest ()

data CheckResult = CheckSuccess | CheckFailure {failedAfter :: Int, failingTest :: FailingTest}
  deriving (Show)

check :: Specification Proposition -> IO ()
check spec = do
  -- stdGen <- getStdGen
  let numTests = 10
  logInfo ("Running " <> show numTests <> " tests...")
  result <- runWebDriver (Pipes.runEffect (runAll numTests spec'))
  case result of
    CheckFailure {failedAfter, failingTest} -> do
      logInfo . renderString $
        prettyTrace (annotateStutteringSteps (trace failingTest)) <> line
      assertFailure ("Failed after " <> show failedAfter <> " tests and " <> show (numShrinks failingTest) <> " shrinks.")
    CheckSuccess -> logInfo ("Passed " <> show numTests <> " tests.")
  where
    spec' = spec & field @"proposition" %~ simplify

runSingle :: Specification Proposition -> Int -> Runner (Either FailingTest ())
runSingle spec' size = do
  actions <- inNewPrivateWindow (genActions spec' size)
  (original, result) <- inNewPrivateWindow (runAndVerify spec' actions)
  case result of
    Accepted -> pure (Right ())
    Rejected -> do
      logInfoWD "Test failed. Shrinking..."
      fromMaybe (Left (FailingTest 0 original)) <$> shrinkFailing spec' actions 1

runAll :: Int -> Specification Proposition -> Effect Runner CheckResult
runAll numTests spec' = (allTests $> CheckSuccess) >-> firstFailure
  where
    runSingle' :: Int -> Producer (Either FailingTest ()) Runner ()
    runSingle' size = do
      lift (logInfoWD ("Running test with size: " <> show size))
      Pipes.yield =<< lift (runSingle spec' size)
    allTests :: Producer (Either FailingTest (), Int) Runner ()
    allTests = Pipes.for (sizes numTests) runSingle' `Pipes.zip` Pipes.each [1 ..]

-- pure CheckSuccess

firstFailure :: Functor m => Consumer (Either FailingTest (), Int) m CheckResult
firstFailure =
  Pipes.await >>= \case
    (Right {}, _) -> firstFailure
    (Left failingTest, n) -> pure (CheckFailure n failingTest)

sizes :: Functor m => Int -> Producer Int m ()
sizes numSizes = Pipes.each (map (\n -> (n * 100 `div` numSizes)) [1 .. numSizes])

{-
generateAndVerify :: Specification Proposition -> Runner ([Action Selected], Trace TraceElementEffect, Result)
generateAndVerify spec = do
  -- trace <- annotateStutteringSteps <$> runActions spec actions
  (actions, trace) <- _
  pure (actions, trace, verify (trace ^.. nonStutterStates) (proposition spec))
-}

shrinkFailing :: Specification Proposition -> [Action Selected] -> Int -> Runner (Maybe TestResult)
shrinkFailing spec original n
  | n <= 100 = do
    logInfoWD . renderString $ "Shrink #" <> pretty n <> "..."
    go (shrink original)
  | otherwise = pure Nothing
  where
    go = \case
      [] -> pure Nothing
      ([] : rest) -> go rest
      (actions : rest) ->
        inNewPrivateWindow (runAndVerify spec actions) >>= \case
          (_, Accepted) -> go rest
          (trace, Rejected) -> (<|> Just (Left (FailingTest n trace))) <$> shrinkFailing spec actions (succ n)
    shrink = QuickCheck.shrinkList shrinkAction

{-# SCC runAndVerify "runAndVerify" #-}
runAndVerify :: Specification Proposition -> [Action Selected] -> Runner (Trace TraceElementEffect, Result)
runAndVerify spec actions = do
  trace <- annotateStutteringSteps <$> runActions spec actions
  pure (trace, verify (trace ^.. nonStutterStates) (proposition spec))

-- TODO?
shrinkAction :: Action sel -> [Action sel]
shrinkAction = const []

validActions :: [(Int, Action Selector)] -> Runner (Maybe (QuickCheck.Gen (Action Selected)))
validActions actions = do
  gens <- catMaybes <$> traverse tryGenActionWithFreq actions
  case gens of
    [] -> pure Nothing
    _ -> pure (Just (QuickCheck.frequency gens))
  where
    tryGenActionWithFreq :: (Int, Action Selector) -> Runner (Maybe (Int, QuickCheck.Gen (Action Selected)))
    tryGenActionWithFreq (i, a) = fmap (i,) <$> tryGenAction a
    tryGenAction :: Action Selector -> Runner (Maybe (QuickCheck.Gen (Action Selected)))
    tryGenAction = \case
      KeyPress k ->
        activeElement >>= \case
          Just el ->
            getElementTagName el >>= \case
              name
                | name `elem` ["input", "textarea"] -> pure (Just (pure (KeyPress k)))
                | otherwise -> pure Nothing
          Nothing -> pure Nothing
      Navigate p -> pure (Just (pure (Navigate p)))
      Focus sel -> selectOne sel Focus (isNotActive . toRef)
      Click sel -> selectOne sel Click isElementVisible
    selectOne :: Selector -> (Selected -> Action Selected) -> (Element -> Runner Bool) -> Runner (Maybe (QuickCheck.Gen (Action Selected)))
    selectOne sel ctor isValid = do
      found <- findAll sel
      validChoices <-
        ( filterM
            (\(_, e) -> isValid e `catchError` (const (pure False)))
            (zip [0 ..] found)
          )
      case validChoices of
        [] -> pure Nothing
        choices -> do
          pure (Just (ctor . Selected sel <$> QuickCheck.elements (map fst choices)))
    isNotActive e = (/= Just e) <$> activeElement
    activeElement = (Just <$> getActiveElement) `catchError` const (pure Nothing)

{-# SCC genActions "genActions" #-}
genActions :: Specification Proposition -> Int -> Runner [Action Selected]
genActions spec maxNum = do
  navigateToOrigin spec
  initializeScript
  awaitElement (readyWhen spec)
  go []
  where
    go acc
      | length acc < maxNum = do
        validActions (actions spec) >>= \case
          Just genValidAction -> do
            next <- (liftWebDriverTT (lift (QuickCheck.generate genValidAction)))
            (runAction next) >>= \case
              ActionFailed err ->
                logInfoWD . renderString $
                  "Action" <+> prettyAction next <+> "considered valid but did not run successfully:" <+> pretty err
              ActionImpossible ->
                logInfoWD . renderString $
                  "Action" <+> prettyAction next <+> "considered impossible to run."
              ActionSuccess -> pure ()
            go (acc <> [next])
          Nothing -> pure acc
      | otherwise = pure acc

navigateToOrigin :: Specification formula -> Runner ()
navigateToOrigin spec = case origin spec of
  Path path -> (navigateTo (Text.unpack path))

runActions :: Specification Proposition -> [Action Selected] -> Runner (Trace ())
runActions spec actions = do
  navigateToOrigin spec
  initializeScript
  awaitElement (readyWhen spec)
  initial <- observe
  rest <- concat <$> traverse runActionAndObserve actions
  pure (Trace (initial : rest))
  where
    runActionAndObserve action = do
      result <- (runAction action)
      s <- observe
      pure [TraceAction () action result, s]
    observe = do
      values <-
        withQueries runQuery (proposition spec)
          & evalState RunQueryState {cachedElements = mempty, cachedElementStates = mempty}
          & runM
      let (queriedElements, elementStates) =
            bimap groupUniqueIntoMap groupUniqueIntoMap (partitionEithers (concat values))
      pure (TraceState () (ObservedState {queriedElements, elementStates}))

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

inNewPrivateWindow :: Runner ~> Runner
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

groupUniqueIntoMap :: (Eq a, Hashable a, Eq b) => [(a, b)] -> HashMap a [b]
groupUniqueIntoMap = HashMap.map nub . HashMap.fromListWith (++) . map (\(k, v) -> (k, [v]))

type QueriedElement = (Selector, Element)

type QueriedElementState = (Element, ElementStateValue)

type QueriedValue = Either QueriedElement QueriedElementState

data RunQueryState
  = RunQueryState
      { cachedElements :: HashMap Selector [Element],
        cachedElementStates :: HashMap (Element, SomeElementState) ElementStateValue
      }
  deriving (Generic)

runQuery :: Query a -> Eff '[State RunQueryState, Runner] [Either QueriedElement QueriedElementState]
runQuery (Query query') =
  fmap snd
    $ runWriter
    $ reinterpret3 go query'
  where
    go :: QueryF ~> Eff '[Writer [QueriedValue], State RunQueryState, Runner]
    go =
      ( \case
          QueryAll selector ->
            useCachedOrInsert selector (field @"cachedElements") id pure do
              els <- sendM (findAll selector)
              tell ((Left . (selector,) <$> els) :: [QueriedValue])
              pure els
          Get state el -> do
            useCachedOrInsert (el, SomeElementState state) (field @"cachedElementStates") (ElementStateValue state) (\(ElementStateValue _ x) -> cast x) do
              value <- sendM $ case state of
                Attribute name -> (either (const name) Text.pack) <$> (getElementAttribute (Text.unpack name) (toRef el))
                Property name -> (getElementProperty (Text.unpack name) (toRef el))
                CssValue name -> Text.pack <$> (getElementCssValue (Text.unpack name) (toRef el))
                Text -> Text.pack <$> (getElementText (toRef el))
                Enabled -> (isElementEnabled (toRef el))
              tell [Right (el, ElementStateValue state value) :: QueriedValue]
              pure value
      )
    useCachedOrInsert ::
      ( Eq k,
        Hashable k,
        Member (State RunQueryState) effs
      ) =>
      k ->
      (Lens' RunQueryState (HashMap k a)) ->
      (b -> a) ->
      (a -> Maybe b) ->
      Eff effs b ->
      Eff effs b
    useCachedOrInsert key cached toValue fromValue query = do
      s <- get
      case s ^. cached . at key >>= fromValue of
        Just x -> pure x
        Nothing -> do
          x <- query
          put (s & cached . at key ?~ toValue x)
          pure x

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn

logInfoWD :: String -> Runner ()
logInfoWD = liftWebDriverTT . logInfo

isElementVisible :: Element -> Runner Bool
isElementVisible el = do
  r <- (== JSON.Bool True) <$> executeScript "return window.wtp.isElementVisible(arguments[0])" [JSON.toJSON el]
  logInfoWD (show el <> " visible: " <> show r)
  pure r

awaitElement :: Selector -> WebDriverT IO ()
awaitElement (Selector sel) =
  void (executeAsyncScript "window.wtp.awaitElement(arguments[0], arguments[1])" [JSON.toJSON sel])

renderString :: Doc AnsiStyle -> String
renderString = Text.unpack . renderStrict . layoutPretty defaultLayoutOptions

wtpJs :: Script
wtpJs = $(embedStringFile' "lib/wtp.js")

initializeScript :: Runner ()
initializeScript =
  void (executeScript wtpJs [])
