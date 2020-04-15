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

import Control.Lens ((%~))
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
import qualified Hedgehog
import qualified WTP.Formula.NNF as NNF
import qualified WTP.Formula.Syntax as Syntax
import WTP.Query
import WTP.Result
import WTP.Specification
import WTP.Verify
import Web.Api.WebDriver hiding (Selector, runIsolated)
import qualified Data.Tree as Tree

type Runner = Hedgehog.PropertyT (WebDriverTT IdentityT IO)

asProperty :: Specification Syntax.Formula -> Hedgehog.Property
asProperty spec = Hedgehog.property . hoist runWebDriver $ do
  let spec' = spec & field @"property" %~ Syntax.toNNF
  steps <- hoist (runIsolated defaultFirefoxCapabilities) (runSpec spec')
  let result = NNF.verifyWith assertQuery (property spec') steps
  case result of
    Accepted -> pure ()
    Rejected -> do
      Hedgehog.footnote (unlines (map (Tree.drawTree . drawStep) steps))
      Hedgehog.failure

runSpec :: Specification NNF.Formula -> Runner [Step]
runSpec spec = do
  navigateToOrigin
  (:) <$> buildStep <*> traverse (\action -> runAction action >> buildStep) (actions spec)
  where
    extractQueries = NNF.withQueries runQuery (property spec)
    navigateToOrigin = case origin spec of
      Path path -> lift (navigateTo (Text.unpack path))
    runAction = \case
      Focus s -> find1 s >>= lift . elementSendKeys ""
      KeyPress c -> lift (getActiveElement >>= elementSendKeys [c])
      Click s -> find1 s >>= lift . elementClick
      Navigate (Path path) -> lift (navigateTo (Text.unpack path))
    buildStep = do
      values <- Eff.runM extractQueries
      let (queriedElements, elementStates) =
            bimap groupUniqueIntoMap groupUniqueIntoMap (partitionEithers (concat values))
      pure (Step {queriedElements, elementStates})

runWebDriver :: WebDriverT IO a -> IO a
runWebDriver ma = do
  execWebDriverT defaultWebDriverConfig ma >>= \case
    (Right x, _, _) -> pure x
    (Left err, _, _) -> fail (show err)

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

-- | Same as the non-exported definition in 'Web.Api.WebDriver.Endpoints'.
find1 :: Selector -> Runner ElementRef
find1 (Selector s) = lift (findElement CssSelector (Text.unpack s))

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
          Get state element -> do
            value <- Eff.sendM $ case state of
              Attribute name -> fmap Text.pack <$> lift (getElementAttribute (Text.unpack name) (toRef element))
              Property name -> lift (getElementProperty (Text.unpack name) (toRef element))
              CssValue name -> Text.pack <$> lift (getElementCssValue (Text.unpack name) (toRef element))
              Text -> Text.pack <$> lift (getElementText (toRef element))
              Enabled -> lift (isElementEnabled (toRef element))
            tell [Right (element, ElementStateValue state value) :: Either QueriedElement QueriedElementState]
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
