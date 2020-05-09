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
import Control.Monad (filterM)
import Control.Monad.Freer (Eff, reinterpret2, runM, sendM, type (~>))
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Identity (IdentityT)
import qualified Data.Aeson as JSON
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either (partitionEithers)
import Data.Function ((&))
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
import qualified Test.QuickCheck as QuickCheck
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (assertFailure, testCase)
import WTP.Element
import WTP.Formula
import WTP.Query
import WTP.Result
import WTP.Specification
import WTP.Trace
import WTP.Verify
import Web.Api.WebDriver hiding (Action, Selector, assertFailure, hPutStrLn, runIsolated)

type WD = WebDriverTT IdentityT IO

type Runner = WD -- Eff '[WD]

testSpecifications :: [(Text, Specification Proposition)] -> Tasty.TestTree
testSpecifications specs =
  Tasty.testGroup "WTP specifications" [testCase (Text.unpack name) (test spec) | (name, spec) <- specs]

data FailingTest = FailingTest {numShrinks :: Int, trace :: Trace TraceElementEffect}

type TestResult = Either FailingTest ()

data CheckResult = CheckSuccess | CheckFailure {failedAfter :: Int, failingTest :: FailingTest}

test :: Specification Proposition -> IO ()
test spec = do
  -- stdGen <- getStdGen
  let numTests = 10
  let sizes = map (\n -> n * 100 `div` numTests) [1 .. numTests]
  logInfo ("Running " <> show numTests <> " tests...")
  result <- runWebDriver (runAll sizes 1)
  case result of
    CheckFailure {failedAfter, failingTest} -> do
      logInfo . renderString $
        prettyTrace (annotateStutteringSteps (trace failingTest)) <> line
      assertFailure ("Failed after " <> show failedAfter <> " tests and " <> show (numShrinks failingTest) <> " shrinks.")
    CheckSuccess -> logInfo ("Passed " <> show numTests <> " tests.")
  where
    runSingle size = do
      actions <- inNewPrivateWindow (genActions spec' size)
      logInfoWD (Text.unpack (renderStrict (layoutPretty defaultLayoutOptions ("Running and verifying" <+> pretty (length actions) <+> "actions:" <> line <+> annotate (colorDull Black) (prettyActions actions)))))
      (original, result) <- inNewPrivateWindow (runAndVerify spec' actions)
      case result of
        Accepted -> pure (Right ())
        Rejected -> do
          logInfoWD "Test failed. Shrinking..."
          fromMaybe (Left (FailingTest 0 original)) <$> shrinkFailing spec' actions 1
    runAll [] _ = pure CheckSuccess
    runAll (size : sizes) (n :: Int) = do
      logInfoWD ("Running test " <> show n <> " with size: " <> show size)
      runSingle size >>= \case
        Right {} -> runAll sizes (succ n)
        Left failingTest -> pure (CheckFailure n failingTest)
    spec' = spec & field @"proposition" %~ simplify

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
      Focus sel -> selectOne sel Focus isNotActive
      Click sel -> selectOne sel Click isElementVisible
    selectOne :: Selector -> (Selected -> Action Selected) -> (ElementRef -> WD Bool) -> Runner (Maybe (QuickCheck.Gen (Action Selected)))
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

genActions :: Specification Proposition -> Int -> Runner [Action Selected]
genActions spec maxNum = do
  navigateToOrigin spec
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
  -- lift breakpointsOn
  navigateToOrigin spec
  awaitElement (readyWhen spec)
  initial <- observe
  rest <- concat <$> traverse runActionAndObserve actions
  pure (Trace (initial : rest))
  where
    queries = withQueries runQuery (proposition spec)
    runActionAndObserve action = do
      result <- (runAction action)
      s <- observe
      pure [TraceAction () action result, s]
    observe = do
      values <- queries
      let (queriedElements, elementStates) =
            bimap groupUniqueIntoMap groupUniqueIntoMap (partitionEithers (concat values))
      pure (TraceState () (ObservedState {queriedElements, elementStates}))

tryAction :: WD ActionResult -> WD ActionResult
tryAction action = action `catchError` (pure . ActionFailed . Text.pack . show)

click :: Selected -> WD ActionResult
click = findSelected >=> \case
  Just e -> tryAction (ActionSuccess <$ (elementClick e))
  Nothing -> pure ActionImpossible

sendKey :: Char -> WD ActionResult
sendKey c = tryAction (ActionSuccess <$ (getActiveElement >>= elementSendKeys [c]))

focus :: Selected -> WD ActionResult
focus = findSelected >=> \case
  Just e -> tryAction (ActionSuccess <$ (elementSendKeys "" e))
  Nothing -> pure ActionImpossible

runAction :: Action Selected -> WD ActionResult
runAction = \case
  Focus s -> focus s
  KeyPress c -> sendKey c
  Click s -> click s
  Navigate (Path path) -> tryAction (ActionSuccess <$ navigateTo (Text.unpack path))

runWebDriver :: WD a -> IO a
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

inNewPrivateWindow :: Runner ~> Runner
inNewPrivateWindow = (runIsolated (reconfigure headlessFirefoxCapabilities))
  where
    reconfigure c =
      c
        { _firefoxOptions = (_firefoxOptions c)
            <&> \o ->
              o
                { _firefoxArgs = Just ["-headless", "-private"],
                  _firefoxPrefs = Just (HashMap.singleton "Dom.storage.enabled" (JSON.Bool False))
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

findSelected :: Selected -> WD (Maybe ElementRef)
findSelected (Selected s i) =
  findAll s >>= \case
    es
      | length es > i -> pure (Just (es !! i))
      | otherwise -> pure Nothing

findAll :: Selector -> WD [ElementRef]
findAll (Selector s) = findElements CssSelector (Text.unpack s)

toRef :: Element -> ElementRef
toRef (Element ref) = ElementRef (Text.unpack ref)

fromRef :: ElementRef -> Element
fromRef (ElementRef ref) = Element (Text.pack ref)

groupUniqueIntoMap :: (Eq a, Hashable a, Eq b) => [(a, b)] -> HashMap a [b]
groupUniqueIntoMap = HashMap.map nub . HashMap.fromListWith (++) . map (\(k, v) -> (k, [v]))

type QueriedElement = (Selector, Element)

type QueriedElementState = (Element, ElementStateValue)

runQuery :: Query a -> Runner [Either QueriedElement QueriedElementState]
runQuery (Query query') =
  fmap snd
    $ runM
    $ runWriter
    $ reinterpret2 go query'
  where
    go :: QueryF ~> Eff '[Writer [Either (Selector, Element) (Element, ElementStateValue)], WebDriverTT IdentityT IO]
    go =
      ( \case
          QueryAll selector -> do
            els <- fmap fromRef <$> sendM (findAll selector)
            tell ((Left . (selector,) <$> els) :: [Either QueriedElement QueriedElementState])
            pure els
          Get state el -> do
            value <- sendM $ case state of
              Attribute name -> (either (const name) Text.pack) <$> (getElementAttribute (Text.unpack name) (toRef el))
              Property name -> (getElementProperty (Text.unpack name) (toRef el))
              CssValue name -> Text.pack <$> (getElementCssValue (Text.unpack name) (toRef el))
              Text -> Text.pack <$> (getElementText (toRef el))
              Enabled -> (isElementEnabled (toRef el))
            tell [Right (el, ElementStateValue state value) :: Either QueriedElement QueriedElementState]
            pure value
      )

isElementVisible :: ElementRef -> WD Bool
isElementVisible el = do
  enabled <- isElementEnabled el
  offsetParent <- getElementProperty "offsetParent" el
  d <- getElementCssValue "display" el
  v <- getElementCssValue "visibility" el
  o <- getElementCssValue "opacity" el
  pure (enabled && offsetParent /= JSON.Null && d /= "none" && v /= "hidden" && o /= "0")

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn

logInfoWD :: String -> Runner ()
logInfoWD = liftWebDriverTT . logInfo

awaitElement :: Selector -> WebDriverT IO ()
awaitElement (Selector sel) =
  void
    ( executeAsyncScript
        " var sel = arguments[0]; \
        \ var done = arguments[1]; \
        \ var timer = setInterval(function () { \
        \   if (document.querySelector(sel)) { clearInterval(timer); done(); } \
        \ }, 100); \
        \"
        [JSON.toJSON sel]
    )

renderString :: Doc AnsiStyle -> String
renderString = Text.unpack . renderStrict . layoutPretty defaultLayoutOptions
