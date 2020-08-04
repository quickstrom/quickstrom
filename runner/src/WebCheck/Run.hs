{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WebCheck.Run
  ( CheckOptions (..),
    CheckResult (..),
    FailingTest (..),
    Size (..),
    CheckEvent (..),
    TestEvent (..),
    check,
  )
where

import Control.Lens hiding (each)
import Control.Monad ((>=>), fail, forever, void, when)
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Loops (andM)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Data.Aeson as JSON
import Data.Function ((&))
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.String (String, fromString)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Tree
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Network.HTTP.Client as Http
import qualified Network.Wreq as Wreq
import Pipes ((>->), Pipe, Producer)
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified Test.QuickCheck as QuickCheck
import Text.URI (URI)
import qualified Text.URI as URI
import Web.Api.WebDriver hiding (Action, LogLevel (..), Selector, Timeout, hPutStrLn, runIsolated)
import qualified Web.Api.WebDriver as WebDriver
import WebCheck.Element
import WebCheck.LogLevel
import WebCheck.Prelude hiding (catchError, check, throwError, trace)
import WebCheck.Result
import WebCheck.Specification
import WebCheck.Trace

type Runner = WebDriverTT (ReaderT CheckEnv) IO

data FailingTest
  = FailingTest
      { numShrinks :: Int,
        trace :: Trace TraceElementEffect,
        reason :: Maybe Text
      }
  deriving (Show, Generic, JSON.ToJSON)

data CheckResult = CheckSuccess | CheckFailure {failedAfter :: Int, failingTest :: FailingTest}
  deriving (Show, Generic, JSON.ToJSON)

data TestEvent
  = TestStarted Size
  | TestPassed Size (Trace TraceElementEffect)
  | TestFailed Size (Trace TraceElementEffect)
  | Shrinking Int
  | RunningShrink Int
  deriving (Show, Generic, JSON.ToJSON)

data CheckEvent
  = CheckStarted Int
  | CheckTestEvent TestEvent
  | CheckFinished CheckResult
  deriving (Show, Generic, JSON.ToJSON)

data CheckEnv = CheckEnv {checkOptions :: CheckOptions, checkScripts :: CheckScripts}

data CheckOptions
  = CheckOptions
      { checkTests :: Int,
        checkMaxActions :: Size,
        checkShrinkLevels :: Int,
        checkOrigin :: URI,
        checkMaxTrailingStateChanges :: Int,
        checkWebDriverLogLevel :: LogLevel
      }

newtype Size = Size {unSize :: Word32}
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

newtype Timeout = Timeout Word64
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

mapTimeout :: (Word64 -> Word64) -> Timeout -> Timeout
mapTimeout f (Timeout ms) = Timeout (f ms)

newtype ObserverId = ObserverId Text
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data CheckScripts
  = CheckScripts
      { isElementVisible :: Element -> Runner Bool,
        observeState :: Queries -> Runner ObservedState,
        registerNextStateObserver :: Timeout -> Queries -> Runner ObserverId,
        awaitNextState :: ObserverId -> Runner ObservedState
      }

check :: (MonadIO m, Specification spec) => CheckOptions -> spec -> Pipes.Producer CheckEvent m CheckResult
check opts@CheckOptions {checkTests} spec = do
  -- stdGen <- getStdGen
  Pipes.yield (CheckStarted checkTests)
  env <- CheckEnv opts <$> lift readScripts
  res <- Pipes.hoist (liftIO . runWebDriver env) (runAll opts spec)
  Pipes.yield (CheckFinished res)
  pure res

elementsToTrace :: Producer (TraceElement ()) Runner () -> Runner (Trace ())
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

runSingle :: Specification spec => spec -> Size -> Producer TestEvent Runner (Either FailingTest ())
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
      CheckOptions {checkShrinkLevels} <- lift (liftWebDriverTT (asks checkOptions))
      Pipes.yield (TestFailed size trace)
      if (checkShrinkLevels > 0)
        then do
          Pipes.yield (Shrinking checkShrinkLevels)
          let shrinks = shrinkForest (QuickCheck.shrinkList shrinkAction) checkShrinkLevels (trace ^.. traceActions)
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
    runAndVerifyIsolated :: Int -> Producer (Action Selected) Runner () -> Producer TestEvent Runner (Either FailingTest (Trace TraceElementEffect))
    runAndVerifyIsolated n producer = do
      trace <- lift do
        opts <- liftWebDriverTT (asks checkOptions)
        annotateStutteringSteps <$> inNewPrivateWindow opts do
          beforeRun spec
          elementsToTrace (producer >-> runActions' spec)
      case verify spec (trace ^.. nonStutterStates) of
        Right Accepted -> pure (Right trace)
        Right Rejected -> pure (Left (FailingTest n trace Nothing))
        Left err -> pure (Left (FailingTest n trace (Just err)))
    runShrink (Shrink n actions') = do
      Pipes.yield (RunningShrink n)
      runAndVerifyIsolated n (Pipes.each actions')

runAll :: Specification spec => CheckOptions -> spec -> Producer CheckEvent Runner CheckResult
runAll opts spec' = untilFirstFailure
  where
    untilFirstFailure :: Producer CheckEvent Runner CheckResult
    untilFirstFailure = go (sizes opts `zip` [1 ..])
      where
        go :: [(Size, Int)] -> Producer CheckEvent Runner CheckResult
        go [] = pure CheckSuccess
        go ((size, n) : rest) =
          (runSingle spec' size >-> Pipes.map CheckTestEvent) >>= \case
            Right {} -> go rest
            Left failingTest -> pure (CheckFailure n failingTest)

sizes :: CheckOptions -> [Size]
sizes CheckOptions {checkMaxActions = Size maxActions, checkTests} =
  map (\n -> Size (n * maxActions `div` fromIntegral checkTests)) [1 .. fromIntegral checkTests]

beforeRun :: Specification spec => spec -> Runner ()
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

observeManyStatesAfter :: Queries -> Action Selected -> Pipe a (TraceElement ()) Runner ()
observeManyStatesAfter queries' action = do
  CheckEnv {checkScripts = scripts, checkOptions = CheckOptions {checkMaxTrailingStateChanges}} <- lift (liftWebDriverTT ask)
  observer <- lift (registerNextStateObserver scripts (Timeout 100) queries')
  result <- lift (runAction action)
  newState <- lift (awaitNextState scripts observer)
  Pipes.yield (TraceAction () action result)
  Pipes.yield (TraceState () newState)
  nonStutters <-
    (loop (Timeout 100) >-> takeWhileChanging >-> Pipes.take checkMaxTrailingStateChanges)
      & Pipes.toListM
      & lift
  mapM_ (Pipes.yield . (TraceState ())) nonStutters
  where
    loop :: Timeout -> Producer ObservedState Runner ()
    loop timeout = do
      scripts <- lift (liftWebDriverTT (asks checkScripts))
      newState <- lift (awaitNextState scripts =<< registerNextStateObserver scripts timeout queries')
      Pipes.yield newState
      loop (mapTimeout (* 2) timeout)

{-# SCC runActions' "runActions'" #-}
runActions' :: Specification spec => spec -> Pipe (Action Selected) (TraceElement ()) Runner ()
runActions' spec = do
  scripts <- lift (liftWebDriverTT (asks checkScripts))
  state1 <- lift (observeState scripts queries')
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

generate :: QuickCheck.Gen a -> Runner a
generate = liftWebDriverTT . lift . QuickCheck.generate

generateValidActions :: Vector (Int, Action Selector) -> Producer (Action Selected) Runner ()
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

selectValidAction :: Action Selector -> Runner (Maybe (Action Selected))
selectValidAction possibleAction =
  case possibleAction of
    KeyPress k -> do
      active <- isActiveInput
      if active then (pure (Just (KeyPress k))) else pure Nothing
    EnterText t -> do
      active <- isActiveInput
      if active then (pure (Just (EnterText t))) else pure Nothing
    Navigate p -> pure (Just (Navigate p))
    Focus sel -> selectOne sel Focus (isNotActive . toRef)
    Click sel -> selectOne sel Click isClickable
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
    isClickable e = do
      scripts <- liftWebDriverTT (asks checkScripts)
      andM [isElementEnabled (toRef e), isElementVisible scripts e]
    isActiveInput =
      activeElement >>= \case
        Just el -> (`elem` ["input", "textarea"]) <$> getElementTagName el
        Nothing -> pure False

navigateToOrigin :: Runner ()
navigateToOrigin = do
  CheckOptions {checkOrigin} <- liftWebDriverTT (asks checkOptions)
  navigateTo (toS (URI.renderStr checkOrigin))

tryAction :: Runner ActionResult -> Runner ActionResult
tryAction action =
  action
    `catchError` ( \case
                     ResponseError _ msg _ _ _ -> do
                       pure (ActionFailed (toS msg))
                     err -> throwError err
                 )

click :: Selected -> Runner ActionResult
click = findSelected >=> \case
  Just e -> tryAction (ActionSuccess <$ (elementClick (toRef e)))
  Nothing -> pure ActionImpossible

sendKeys :: Text -> Runner ActionResult
sendKeys t = tryAction (ActionSuccess <$ (getActiveElement >>= elementSendKeys (toS t)))

sendKey :: Char -> Runner ActionResult
sendKey = sendKeys . Text.singleton

focus :: Selected -> Runner ActionResult
focus = findSelected >=> \case
  Just e -> tryAction (ActionSuccess <$ (elementSendKeys "" (toRef e)))
  Nothing -> pure ActionImpossible

runAction :: Action Selected -> Runner ActionResult
runAction = \case
  Focus s -> focus s
  KeyPress c -> sendKey c
  EnterText t -> sendKeys t
  Click s -> click s
  Navigate uri -> tryAction (ActionSuccess <$ navigateTo (toS uri))

runWebDriver :: CheckEnv -> Runner a -> IO a
runWebDriver opts ma = do
  mgr <- Http.newManager defaultManagerSettings
  let httpOptions :: Wreq.Options
      httpOptions = Wreq.defaults & Wreq.manager .~ Right mgr
  runReaderT (execWebDriverTT (reconfigure defaultWebDriverConfig httpOptions) ma) opts >>= \case
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

inNewPrivateWindow :: CheckOptions -> Runner a -> Runner a
inNewPrivateWindow CheckOptions {checkWebDriverLogLevel} =
  runIsolated (reconfigure headlessFirefoxCapabilities)
  where
    reconfigure c =
      c
        { _firefoxOptions = (_firefoxOptions c)
            <&> \o ->
              o
                { _firefoxArgs = Just ["-headless", "-private"],
                  _firefoxPrefs = Just (HashMap.singleton "Dom.storage.enabled" (JSON.Bool False)),
                  _firefoxLog = Just (FirefoxLog (Just (webdriverLogLevel checkWebDriverLogLevel)))
                }
        }
    webdriverLogLevel = \case
      LogDebug -> WebDriver.LogDebug
      LogInfo -> WebDriver.LogInfo
      LogWarn -> WebDriver.LogWarn
      LogError -> WebDriver.LogError

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
  findAll s >>= \es -> pure ((es ^? ix i))

findAll :: Selector -> Runner [Element]
findAll (Selector s) = map fromRef <$> findElements CssSelector (Text.unpack s)

toRef :: Element -> ElementRef
toRef (Element ref) = ElementRef (Text.unpack ref)

fromRef :: ElementRef -> Element
fromRef (ElementRef ref) = Element (Text.pack ref)

awaitElement :: Selector -> Runner ()
awaitElement sel = do
  let loop = do
        findAll sel >>= \case
          [] -> liftWebDriverTT (liftIO (threadDelay 1000000)) >> loop
          _ -> pass
  loop

executeScript' :: JSON.FromJSON r => Script -> [JSON.Value] -> Runner r
executeScript' script args = do
  r <- executeAsyncScript script args
  case JSON.fromJSON r of
    JSON.Success (Right a) -> pure a
    JSON.Success (Left e) -> fail e
    JSON.Error e -> fail e

readScripts :: MonadIO m => m CheckScripts
readScripts = do
  let key = "WEBCHECK_CLIENT_SIDE_DIR"
  dir <- liftIO (maybe (fail (key <> " environment variable not set")) pure =<< lookupEnv key)
  let readScript :: MonadIO m => JSON.FromJSON a => String -> m ([JSON.Value] -> Runner a)
      readScript name = do
        code <- liftIO (fromString . toS <$> readFile (dir </> name <> ".js"))
        pure (executeScript' code)
  isElementVisibleScript <- readScript "isElementVisible"
  observeStateScript <- readScript "observeState"
  registerNextStateObserverScript <- readScript "registerNextStateObserver"
  awaitNextStateScript <- readScript "awaitNextState"
  pure
    CheckScripts
      { isElementVisible = \el -> (== JSON.Bool True) <$> isElementVisibleScript [JSON.toJSON el],
        observeState = \queries' -> observeStateScript [JSON.toJSON queries'],
        registerNextStateObserver = \timeout queries' -> registerNextStateObserverScript [JSON.toJSON timeout, JSON.toJSON queries'],
        awaitNextState = \queries' -> awaitNextStateScript [JSON.toJSON queries']
      }
