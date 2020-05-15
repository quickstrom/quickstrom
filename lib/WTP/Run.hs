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

import Control.Concurrent (threadDelay)
import Control.Lens hiding (each)
import Control.Monad ((>=>), forever, void, when)
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Loops (andM)
import Control.Monad.State
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Writer
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
import Data.Tree
import Data.Typeable (cast)
import GHC.Generics (Generic)
import Network.HTTP.Client as Http
import qualified Network.Wreq as Wreq
import Pipes ((>->), Consumer, Effect, Pipe, Producer)
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
  deriving (Show, Generic)

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
        prettyTrace (trace failingTest) <> line
      assertFailure ("Failed after " <> show failedAfter <> " tests and " <> show (numShrinks failingTest) <> " levels of shrinking.")
    CheckSuccess -> logInfo ("Passed " <> show numTests <> " tests.")
  where
    spec' = spec & field @"proposition" %~ simplify

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

runSingle :: Specification Proposition -> Int -> Runner (Either FailingTest ())
runSingle spec size = do
  result <- runAndVerifyIsolated (genActions' spec >-> Pipes.take size) 0
  case result of
    Right () -> pure (Right ())
    f@(Left (FailingTest _ trace)) -> do
      logInfoWD "Test failed. Shrinking..."
      let shrinks = shrinkForest (QuickCheck.shrinkList shrinkAction) (trace ^.. traceActions)
      shrunk <- minBy (lengthOf (field @"trace" . traceElements)) (traverseShrinks runShrink shrinks >-> select (preview _Left) >-> Pipes.take 10)
      pure (fromMaybe f (Left <$> shrunk))
  where
    runAndVerifyIsolated producer n = do
      trace <- annotateStutteringSteps <$> inNewPrivateWindow do
        beforeRun spec
        elementsToTrace (producer >-> runActions' spec)
      case verify (trace ^.. nonStutterStates) (proposition spec) of
        Accepted -> pure (Right ())
        Rejected -> pure (Left (FailingTest n trace))
    runShrink (Shrink n actions) =
      runAndVerifyIsolated (Pipes.each actions) n

runAll :: Int -> Specification Proposition -> Effect Runner CheckResult
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

beforeRun :: Specification Proposition -> Runner ()
beforeRun spec = do
  navigateToOrigin spec
  initializeScript
  awaitElement (readyWhen spec)

{-# SCC genActions' "genActions'" #-}
genActions' :: Specification Proposition -> Producer (Action Selected) Runner ()
genActions' spec = forever do
  genValidAction <- lift (validActions (actions spec))
  Pipes.yield =<< lift (liftWebDriverTT (lift (QuickCheck.generate genValidAction)))

{-# SCC runActions' "runActions'" #-}
runActions' :: Specification Proposition -> Pipe (Action Selected) (TraceElement ()) Runner ()
runActions' spec = do
  Pipes.yield =<< lift observe
  forever do
    action <- Pipes.await
    result <- lift (runAction action)
    Pipes.yield (TraceAction () action result)
    Pipes.yield =<< lift observe
  where
    observe = do
      values <-
        withQueries runQuery (proposition spec)
          & flip evalStateT RunQueryState {cachedElements = mempty, cachedElementStates = mempty}
      let (queriedElements, elementStates) =
            bimap groupUniqueIntoMap groupUniqueIntoMap (partitionEithers (concat values))
      pure (TraceState () (ObservedState {queriedElements, elementStates}))

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
shrinkAction = \case
  Wait range -> Wait <$> QuickCheck.shrink range
  _ -> [] -- TODO?

validActions :: [(Int, Action Selector)] -> Runner (QuickCheck.Gen (Action Selected))
validActions actions = do
  gens <- catMaybes <$> traverse tryGenActionWithFreq actions
  case gens of
    [] -> do
      logInfoWD "No actions applicable. Falling back to generating a waiting action."
      pure (genWait (10, 1000))
    _ -> pure (QuickCheck.frequency gens)
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
      Click sel -> selectOne sel Click isClickable
      Wait range -> pure (Just (genWait range))
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
    genWait (min', max') = do
      n <- QuickCheck.choose (min', max')
      pure (Wait (n, n))
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
  Wait (_, max') -> ActionSuccess <$ liftWebDriverTT (liftIO (threadDelay max')) -- Selected should be a single wait time, not a tuple

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

runQuery :: Query a -> StateT RunQueryState Runner [Either QueriedElement QueriedElementState]
runQuery = fmap snd . runWriterT . go
  where
    go :: Query a -> WriterT [QueriedValue] (StateT RunQueryState Runner) [a]
    go =
      ( \case
          ByCss selector ->
            useCachedOrInsert selector (field @"cachedElements") id pure do
              els <- lift (lift (findAll selector))
              tell ((Left . (selector,) <$> els) :: [QueriedValue])
              pure els
          Get state' sub ->
            go sub >>= mapM \el -> do
              useCachedOrInsert (el, SomeElementState state') (field @"cachedElementStates") (ElementStateValue state') (\(ElementStateValue _ x) -> cast x) do
                value <- lift . lift $ case state' of
                  Attribute name -> (either (const name) Text.pack) <$> (getElementAttribute (Text.unpack name) (toRef el))
                  Property name -> (getElementProperty (Text.unpack name) (toRef el))
                  CssValue name -> Text.pack <$> (getElementCssValue (Text.unpack name) (toRef el))
                  Text -> Text.pack <$> (getElementText (toRef el))
                  Enabled -> (isElementEnabled (toRef el))
                tell [Right (el, ElementStateValue state' value) :: QueriedValue]
                pure value
      )
    useCachedOrInsert ::
      ( Eq k,
        Hashable k,
        MonadState RunQueryState m
      ) =>
      k ->
      (Lens' RunQueryState (HashMap k a)) ->
      (b -> a) ->
      (a -> Maybe b) ->
      m b ->
      m b
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
isElementVisible el =
  (== JSON.Bool True) <$> executeScript "return window.wtp.isElementVisible(arguments[0])" [JSON.toJSON el]

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
