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
  ( asProperty,
    Runner,
    runSpec,
    runIsolated,
  )
where

import Control.Lens
import qualified Control.Monad.Freer as Eff
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Control.Monad.Morph (MFunctor (hoist))
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
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Source as Hedgehog
import qualified WTP.Formula.NNF as NNF
import qualified WTP.Formula.Syntax as Syntax
import WTP.Query
import WTP.Result
import WTP.Specification
import WTP.Trace
import Web.Api.WebDriver hiding (Action, Selector, runIsolated)

type Runner = Hedgehog.PropertyT (WebDriverTT IdentityT IO)

asProperty :: Specification Syntax.Formula -> Hedgehog.Property
asProperty spec = Hedgehog.withFrozenCallStack . Hedgehog.property $ do
  let spec' = spec & field @"property" %~ Syntax.toNNF
  trace <- Hedgehog.evalM (hoist (runWebDriver . runIsolated defaultFirefoxCapabilities) (runSpec spec'))
  let result = NNF.verifyWith assertQuery (property spec') (trace ^.. observedStates)
  case result of
    Accepted -> pure ()
    Rejected -> do
      let t = renderStrict (layoutPretty defaultLayoutOptions (prettyTrace trace))
      Hedgehog.footnote (Text.unpack t)
      Hedgehog.failure

anyActions :: Hedgehog.Gen [Action]
anyActions = (<>) <$> genListOf early <*> genListOf late
  where
    genListOf = Gen.resize 100 . Gen.list (Range.linear 1 10) 
    early =
      Gen.choice
        [ pure (Focus "input[type=text]"),
          KeyPress <$> pure ' ' -- Gen.ascii
        ]
    late =
      Gen.choice
        [ pure (Click "input[type=submit]")
        ]

runSpec :: Specification NNF.Formula -> Runner Trace
runSpec spec = do
  -- lift breakpointsOn
  actions <- Hedgehog.forAll anyActions
  navigateToOrigin
  initial <- observe
  -- lift (liftWebDriverTT (lift (print actions)))
  rest <- concat <$> traverse runActionAndObserve actions
  -- lift (breakpoint "after") 
  pure (Trace (initial : rest))
  where
    queries = NNF.withQueries runQuery (property spec)
    navigateToOrigin = case origin spec of
      Path path -> lift (navigateTo (Text.unpack path))
    runActionAndObserve action = do
      runAction action
      s <- observe
      pure [TraceAction action, s]
    observe = do
      values <- Eff.runM queries
      let (queriedElements, elementStates) =
            bimap groupUniqueIntoMap groupUniqueIntoMap (partitionEithers (concat values))
      pure (TraceState (ObservedState {queriedElements, elementStates}))

click :: ElementRef -> Runner ()
click el = lift (elementClick el `catchError` \_ -> pure ())

runAction :: Action -> Runner ()
runAction = \case
  Focus s -> findMaybe s >>= lift . traverse (elementSendKeys "") >> pure ()
  KeyPress c -> lift (getActiveElement >>= elementSendKeys [c])
  Click s -> findMaybe s >>= traverse click >> pure ()
  Navigate (Path path) -> lift (navigateTo (Text.unpack path))

runWebDriver :: WebDriverT IO a -> IO a
runWebDriver ma =
  execWebDriverT (withoutLogs defaultWebDriverConfig) ma >>= \case
    (Right x, _, _) -> pure x
    (Left err, _, _) -> fail (show err)
  where
    withoutLogs c = c {_environment = (_environment c) {_logEntryPrinter = \_ _ -> Nothing}}

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
            pure [Element "a"]
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
