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

import Control.Lens
import Control.Monad ((>=>), replicateM, void)
import qualified Control.Monad.Freer as Eff
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
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
import Data.Maybe (listToMaybe)
import Data.String (IsString (..))
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Debug.Trace as Debug
import QuickCheck.GenT (GenT, liftGen, runGenT)
import qualified Test.QuickCheck as QuickCheck 
import qualified Test.QuickCheck.Gen as QuickCheck
import qualified Test.QuickCheck.Monadic as QuickCheck
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (assertFailure, testCase)
import qualified WTP.Formula.NNF as NNF
import qualified WTP.Formula.Syntax as Syntax
import WTP.Query
import WTP.Result
import WTP.Specification
import WTP.Trace
import Web.Api.WebDriver hiding (Action, Selector, assertFailure, runIsolated)

type Runner = WebDriverTT IdentityT IO

testSpecifications :: [(Text, Specification Syntax.Formula)] -> Tasty.TestTree
testSpecifications specs =
  Tasty.testGroup "WTP specifications" [testCase (Text.unpack name) (test spec) | (name, spec) <- specs]

test :: Specification Syntax.Formula -> IO ()
test spec = do
  let spec' = spec & field @"property" %~ Syntax.toNNF
  allActions <- runWD =<< QuickCheck.generate (runGenT (genActions spec'))
  QuickCheck.quickCheck . QuickCheck.forAll (QuickCheck.sublistOf allActions) $ \actions -> do
    QuickCheck.ioProperty . runWD $ do
      trace <- runActions spec' actions
      pure $ do
        let result = NNF.verifyWith assertQuery (property spec') (trace ^.. observedStates)
        let t = renderStrict (layoutPretty defaultLayoutOptions (prettyTrace (annotateStutteringSteps trace)))
        QuickCheck.counterexample (Text.unpack t) (result QuickCheck.=== Accepted)

  where
    runWD = runWebDriver . runIsolated headlessFirefoxCapabilities

anyAction :: QuickCheck.Gen Action
anyAction =
  QuickCheck.oneof
    [ pure (Focus "input[type=text]"),
      KeyPress
        <$> QuickCheck.frequency
          [ (1, pure ' '),
            (1, pure '\0'),
            (1, QuickCheck.arbitraryASCIIChar)
          ],
      pure (Click "input[type=submit]")
    ]

genActions :: Specification NNF.Formula -> GenT (WebDriverTT IdentityT IO) [Action]
genActions spec = replicateM 100 $ do
  liftGen anyAction
  where
    queries = NNF.withQueries runQuery (property spec)

navigateToOrigin :: (Monad eff, Monad (t eff), MonadTrans t) => Specification formula -> WebDriverTT t eff ()
navigateToOrigin spec = case origin spec of
  Path path -> navigateTo (Text.unpack path)

runActions :: Specification NNF.Formula -> [Action] -> WebDriverT IO (Trace ())
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
try action = (action `catchError` (const (pure ())))

click :: Selector -> Runner ()
click = findMaybe >=> (\e -> try (void (traverse elementClick e)))

sendKey :: Char -> Runner ()
sendKey c = try (getActiveElement >>= elementSendKeys [c])

focus :: Selector -> Runner ()
focus s = findMaybe s >>= (\e -> try (void (traverse (elementSendKeys "") e)))

runAction :: Action -> Runner ()
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

findAll :: Selector -> Runner [ElementRef]
findAll (Selector s) = (findElements CssSelector (Text.unpack s))

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
            pure [Element "a"]
          Get state el -> do
            value <- Eff.sendM $ case state of
              Attribute name -> fmap Text.pack <$> (getElementAttribute (Text.unpack name) (toRef el))
              Property name -> (getElementProperty (Text.unpack name) (toRef el))
              CssValue name -> Text.pack <$> (getElementCssValue (Text.unpack name) (toRef el))
              Text -> Text.pack <$> (getElementText (toRef el))
              Enabled -> (isElementEnabled (toRef el))
            tell [Right (el, ElementStateValue state value) :: Either QueriedElement QueriedElementState]
            pure value
      )
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
