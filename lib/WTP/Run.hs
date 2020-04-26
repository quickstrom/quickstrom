{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module WTP.Run
  ( testSpecifications,
  )
where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad ((>=>), void)
import qualified Control.Monad.Freer as Eff
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Identity (IdentityT)
import Control.Natural (type (~>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.Random (StdGen, getStdGen)
import qualified Test.QuickCheck as QuickCheck
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (testCase, assertFailure)
import qualified WTP.Formula.NNF as NNF
import qualified WTP.Formula.Syntax as Syntax
import WTP.Query
import WTP.Result
import WTP.Specification
import WTP.Trace
import Web.Api.WebDriver hiding (Action, Selector, assertFailure, hPutStrLn, runIsolated)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (filterM)

type Runner = StateT StdGen (WebDriverTT IdentityT IO)

testSpecifications :: [(Text, Specification Syntax.Formula)] -> Tasty.TestTree
testSpecifications specs =
  Tasty.testGroup "WTP specifications" [testCase (Text.unpack name) (test spec) | (name, spec) <- specs]

data FailingTest = FailingTest { numShrinks :: Int, trace :: Trace () }

type TestResult = Either FailingTest ()

data CheckResult = CheckSuccess | CheckFailure { failedAfter :: Int, failingTest :: FailingTest }

test :: Specification Syntax.Formula -> IO ()
test spec = do
  stdGen <- getStdGen
  let numTests = 20
  let sizes = map (\n -> n * 100 `div` numTests) [1 .. numTests]
  logInfo ("Running " <> show numTests <> " tests...")
  (result, _) <- runWD . flip runStateT stdGen $ runAll sizes 1
  case result of
    CheckFailure {failedAfter, failingTest} -> do
      let t = renderStrict (layoutPretty defaultLayoutOptions (prettyTrace (annotateStutteringSteps (trace failingTest)) <> line))
      logInfo (Text.unpack t)
      assertFailure ("Failed after " <> show failedAfter <> " tests and " <> show (numShrinks failingTest) <> " shrinks.")
    CheckSuccess -> logInfo ("Passed " <> show numTests <> " tests.")
  where
    runSingle size = do
      actions <- genActions spec' size
      logInfoWD (Text.unpack (renderStrict (layoutPretty defaultLayoutOptions ("Running and verifying" <+> pretty (length actions) <+> "actions:" <> line <+> annotate (colorDull Black) (prettyActions actions)))))
      (original, result) <- runAndVerify spec' actions
      case result of
        Accepted -> pure (Right ())
        Rejected -> do
          logInfoWD "Test failed. Shrinking..."
          fromMaybe (Left (FailingTest 0 original)) <$> shrinkFailing spec' actions 1
    runAll [] _ = pure CheckSuccess
    runAll (size : sizes) (n :: Int) = do
      logInfoWD ("Running test " <> show n <> " with size: " <> show size)
      runSingle size >>= \case
        Right{} -> runAll sizes (succ n)
        Left failingTest -> pure (CheckFailure n failingTest)
    runWD = runWebDriver . runIsolated headlessFirefoxCapabilities
    spec' = spec & field @"property" %~ Syntax.toNNF

shrinkFailing :: Specification NNF.Formula -> [Action Selected] -> Int -> Runner (Maybe TestResult)
shrinkFailing spec original n 
  | n < 100 = go (shrink original)
  | otherwise = pure Nothing
  where
    go = \case
      [] -> pure Nothing
      ([] : rest) -> go rest
      (actions : rest) ->
        runAndVerify spec actions >>= \case
          (_, Accepted) -> go rest
          (trace, Rejected) -> (<|> Just (Left (FailingTest n trace))) <$> shrinkFailing spec actions (succ n)
    shrink = QuickCheck.shrinkList shrinkAction

runAndVerify :: Specification NNF.Formula -> [Action Selected] -> Runner (Trace (), Result)
runAndVerify spec actions = do
  let verify trace = NNF.verifyWith assertQuery (property spec) (trace ^.. observedStates)
  trace <- runActions spec actions
  pure (trace, verify trace)

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
      KeyPress k -> pure (Just (pure (KeyPress k)))
      Navigate p -> pure (Just (pure (Navigate p)))
      Focus sel -> selectOne sel Focus (lift . fmap not . isElementSelected)
      Click sel -> selectOne sel Click (lift . isElementEnabled)
    selectOne :: Selector -> (Selected -> Action Selected) -> (ElementRef -> Runner Bool) -> Runner (Maybe (QuickCheck.Gen (Action Selected)))
    selectOne sel ctor isValid = do
      validChoices <- filterM (isValid . snd) . zip [0..] =<< findAll sel
      case validChoices of
        [] -> pure Nothing
        choices -> do
          pure (Just (ctor . Selected sel <$> QuickCheck.elements (map fst choices)))

genActions :: Specification NNF.Formula -> Int -> Runner [Action Selected]
genActions spec maxNum = do
  navigateToOrigin spec
  go []
  where
    go acc
      | length acc < maxNum = do
        validActions (actions spec) >>= \case
          Just genValidAction -> do
            next <- lift (liftWebDriverTT (lift (QuickCheck.generate genValidAction)))
            runAction next
            go (acc <> [next])
          Nothing -> pure acc
      | otherwise = pure acc

navigateToOrigin :: Specification formula -> Runner ()
navigateToOrigin spec = case origin spec of
  Path path -> lift (navigateTo (Text.unpack path))

runActions :: Specification NNF.Formula -> [Action Selected] -> Runner (Trace ())
runActions spec actions = do
  -- lift breakpointsOn
  navigateToOrigin spec
  initial <- observe
  rest <- concat <$> traverse runActionAndObserve actions
  pure (Trace (initial : rest))
  where
    queries = NNF.withQueries runQuery (property spec)
    runActionAndObserve action = do
      runAction action
      s <- observe
      pure [TraceAction () action, s]
    observe = do
      values <- Eff.runM queries
      let (queriedElements, elementStates) =
            bimap groupUniqueIntoMap groupUniqueIntoMap (partitionEithers (concat values))
      pure (TraceState () (ObservedState {queriedElements, elementStates}))

try :: WebDriverT IO () -> Runner ()
try action = lift (action `catchError` (const (pure ())))

click :: Selected -> Runner ()
click = findSelected >=> (\e -> try (void (elementClick e)))

sendKey :: Char -> Runner ()
sendKey c = try (getActiveElement >>= elementSendKeys [c])

focus :: Selected -> Runner ()
focus = findSelected >=> (\e -> try (void (elementSendKeys "" e)))

runAction :: Action Selected -> Runner ()
runAction = \case
  Focus s -> focus s
  KeyPress c -> sendKey c
  Click s -> click s
  Navigate (Path path) -> try (navigateTo (Text.unpack path))

runWebDriver :: WebDriverT IO a -> IO a
runWebDriver ma =
  execWebDriverT (reconfigure defaultWebDriverConfig) ma >>= \case
    (Right x, _, _) -> pure x
    (Left err, _, _) -> fail (show err)
  where
    reconfigure c =
      c
        { _environment =
            (_environment c)
              { _logEntryPrinter = \_ _ -> Nothing
              }
        }

-- | Mostly the same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
runIsolated ::
  (Monad eff, Monad (t eff), MonadTrans t) =>
  Capabilities ->
  WebDriverTT t eff a ->
  WebDriverTT t eff a
runIsolated caps theSession = cleanupOnError $ do
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

findMaybe :: Selector -> Runner (Maybe ElementRef)
findMaybe = fmap listToMaybe . findAll

findSelected :: Selected -> Runner ElementRef
findSelected (Selected s i) = fmap (!! i) (findAll s)

findAll :: Selector -> Runner [ElementRef]
findAll (Selector s) = lift (findElements CssSelector (Text.unpack s))

toRef :: Element -> ElementRef
toRef (Element ref) = ElementRef (Text.unpack ref)

fromRef :: ElementRef -> Element
fromRef (ElementRef ref) = Element (Text.pack ref)

groupUniqueIntoMap :: (Eq a, Hashable a, Eq b) => [(a, b)] -> HashMap a [b]
groupUniqueIntoMap = HashMap.map nub . HashMap.fromListWith (++) . map (\(k, v) -> (k, [v]))

type QueriedElement = (Selector, Element)

type QueriedElementState = (Element, ElementStateValue)

runQuery :: Eff '[Query] a -> Eff '[Runner] [Either QueriedElement QueriedElementState]
runQuery query' =
  fmap snd
    $ runWriter
    $ Eff.reinterpret2 go query'
  where
    go :: Query ~> Eff '[Writer [Either (Selector, Element) (Element, ElementStateValue)], Runner]
    go =
      ( \case
          Query selector -> do
            el <- fmap fromRef <$> Eff.sendM (findMaybe selector)
            case el of
              Just el' -> tell [Left (selector, el') :: Either QueriedElement QueriedElementState]
              Nothing -> pure ()
            pure el
          QueryAll selector -> do
            els <- fmap fromRef <$> Eff.sendM (findAll selector)
            tell ((Left . (selector,) <$> els) :: [Either QueriedElement QueriedElementState])
            pure els
          Get state el -> do
            value <- Eff.sendM $ case state of
              Attribute name -> fmap Text.pack <$> lift (getElementAttribute (Text.unpack name) (toRef el))
              Property name -> lift (getElementProperty (Text.unpack name) (toRef el))
              CssValue name -> Text.pack <$> lift (getElementCssValue (Text.unpack name) (toRef el))
              Text -> Text.pack <$> lift (getElementText (toRef el))
              Enabled -> lift (isElementEnabled (toRef el))
            tell [Right (el, ElementStateValue state value) :: Either QueriedElement QueriedElementState]
            pure value
      )

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn
      
logInfoWD :: String -> Runner ()
logInfoWD = lift . liftWebDriverTT . logInfo

{-
myWait :: Int -> WebDriverT IO ()
myWait ms =
  void
    ( executeAsyncScript
        " var ms = arguments[0]; \
        \ var done = arguments[1]; \
        \ setTimeout(done, ms) \
        \"
        [JSON.toJSON ms]
    )

-}
