{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Quickstrom.Run.Actions
  ( awaitElement,
    defaultTimeout,
    shrinkAction,
    generateValidActions,
    isCurrentlyValid,
    isActionCurrentlyValid,
    runActionSequence,
    reselect,
    chooseAction,
  )
where

import Control.Lens
import Control.Monad (fail)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Loops (andM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.List hiding (map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Pipes (Producer)
import qualified Pipes
import Quickstrom.Action
import Quickstrom.Element
import Quickstrom.Prelude hiding (catch, check, trace)
import Quickstrom.Run.Runner
import Quickstrom.Run.Scripts (CheckScripts (..), runCheckScript)
import Quickstrom.Timeout (Timeout (..))
import Quickstrom.Trace (ActionResult (..))
import Quickstrom.WebDriver.Class
import qualified Test.QuickCheck as QuickCheck

shrinkAction :: ActionSequence Selected -> [ActionSequence Selected]
shrinkAction _ = [] -- TODO?

generate :: MonadIO m => QuickCheck.Gen a -> m a
generate = liftIO . QuickCheck.generate

generateValidActions ::
  (MonadIO m, WebDriver m) =>
  Vector (Weighted (ActionSequence Selector)) ->
  Producer (ActionSequence (Element, Selected)) (Runner m) ()
generateValidActions possibleActions = loop
  where
    loop =
      lift (findActionCandidates possibleActions >>= filterCurrentlyValidActionCandidates >>= chooseAction)
        >>= \case
          Nothing -> pass
          Just action -> Pipes.yield action >> loop

findActionCandidates ::
  (MonadIO m, WebDriver m) =>
  Vector (Weighted (ActionSequence Selector)) ->
  Runner m (Vector (Weighted (ActionSequence (Element, Selected))))
findActionCandidates = map fold . traverse (map (maybe mempty pure) . findActionSequenceCandidate)
  where
    findActionSequenceCandidate :: (MonadIO m, WebDriver m) => Weighted (ActionSequence Selector) -> Runner m (Maybe (Weighted (ActionSequence (Element, Selected))))
    findActionSequenceCandidate (Weighted weight' (Single action)) = map (Weighted weight' . Single) <$> findActionCandidate action
    findActionSequenceCandidate (Weighted weight' (Sequence (action :| actions))) =
      findActionCandidate action >>= \case
        Just candidate -> do
          candidates <-
            traverse
              (maybe (fail ("Cannot find elements to select for all subsequent actions in sequence: " <> show actions)) pure <=< findActionCandidate)
              actions
          pure (pure (Weighted weight' (Sequence (candidate :| candidates))))
        Nothing -> pure Nothing

    findActionCandidate :: (MonadIO m, WebDriver m) => Action Selector -> Runner m (Maybe (Action (Element, Selected)))
    findActionCandidate = \case
      KeyPress k -> pure (pure (KeyPress k))
      EnterText t -> pure (pure (EnterText t))
      Navigate p -> pure (pure (Navigate p))
      Await sel -> pure (pure (Await sel))
      AwaitWithTimeoutSecs i sel -> pure (pure (AwaitWithTimeoutSecs i sel))
      Focus sel -> map Focus <$> findCandidate sel
      Click sel -> map Click <$> findCandidate sel
      Clear sel -> map Clear <$> findCandidate sel
      Refresh -> pure (pure Refresh)

    findCandidate :: (MonadIO m, WebDriver m) => Selector -> Runner m (Maybe (Element, Selected))
    findCandidate sel = do
      found <- findAll sel
      case zip [0 ..] found of
        [] -> pure Nothing
        choices -> Just <$> generate (QuickCheck.elements [(e, Selected sel i) | (i, e) <- choices])

filterCurrentlyValidActionCandidates ::
  (MonadIO m, WebDriver m) =>
  Vector (Weighted (ActionSequence (Element, Selected))) ->
  Runner m (Vector (Weighted (ActionSequence (Element, Selected))))
filterCurrentlyValidActionCandidates = Vector.filterM (isCurrentlyValid . weighted)

isCurrentlyValid :: WebDriver m => ActionSequence (Element, b) -> Runner m Bool
isCurrentlyValid = isActionCurrentlyValid . actionSequenceHead

isActionCurrentlyValid :: WebDriver m => Action (Element, b) -> Runner m Bool
isActionCurrentlyValid = \case
  KeyPress _ -> isActiveInput
  EnterText _ -> isActiveInput
  Navigate _ -> pure True
  Await _el -> pure True
  AwaitWithTimeoutSecs _ _ -> pure True
  Focus (el, _) -> isNotActive el
  Click (el, _) -> isClickable el
  Clear (el, _) -> isClearable el
  Refresh -> pure True
  where
    isNotActive e = (/= Just e) <$> activeElement
    activeElement = (Just <$> getActiveElement) `catchResponseError` const (pure Nothing)
    isClickable e = do
      scripts <- asks checkScripts
      andM [isElementEnabled e, runCheckScript (isElementVisible scripts e)]
    isClearable el = (`elem` ["input", "textarea"]) <$> getElementTagName el
    isActiveInput =
      activeElement >>= \case
        Just el -> (`elem` ["input", "textarea"]) <$> getElementTagName el
        Nothing -> pure False

chooseAction ::
  (MonadIO m) =>
  Vector (Weighted (ActionSequence (Element, Selected))) ->
  Runner m (Maybe (ActionSequence (Element, Selected)))
chooseAction choices
  | Vector.null choices = pure Nothing
  | otherwise = Just <$> generate (QuickCheck.frequency [(w, pure x) | Weighted w x <- Vector.toList choices])

tryAction :: WebDriver m => Runner m ActionResult -> Runner m ActionResult
tryAction action =
  action
    `catchResponseError` (\(WebDriverResponseError msg) -> pure (ActionFailed msg))

click :: WebDriver m => Element -> Runner m ActionResult
click e = tryAction (ActionSuccess <$ elementClick e)

clear :: WebDriver m => Element -> Runner m ActionResult
clear e = tryAction (ActionSuccess <$ elementClear e)

sendKeys :: WebDriver m => Text -> Runner m ActionResult
sendKeys t = tryAction (ActionSuccess <$ (getActiveElement >>= elementSendKeys t))

sendKey :: WebDriver m => Char -> Runner m ActionResult
sendKey = sendKeys . Text.singleton

focus :: WebDriver m => Element -> Runner m ActionResult
focus e = tryAction (ActionSuccess <$ elementSendKeys "" e)

runAction :: (MonadIO m, WebDriver m) => Action Element -> Runner m ActionResult
runAction = \case
  Focus s -> focus s
  KeyPress c -> sendKey c
  EnterText t -> sendKeys t
  Click s -> click s
  Clear s -> clear s
  Await s -> awaitElement defaultTimeout s
  AwaitWithTimeoutSecs i s -> awaitElement (Timeout (fromIntegral i * 1000)) s
  Navigate uri -> tryAction (ActionSuccess <$ navigateTo uri)
  Refresh -> tryAction (ActionSuccess <$ pageRefresh)

runActionSequence :: (MonadIO m, WebDriver m) => ActionSequence Element -> Runner m ActionResult
runActionSequence = \case
  Single h -> runAction h
  Sequence actions' ->
    let loop [] = pure ActionSuccess
        loop (x : xs) =
          runAction x >>= \case
            ActionSuccess -> loop xs
            err -> pure err
     in loop (toList actions')

reselect :: WebDriver m => Selected -> m (Maybe (Element, Selected))
reselect selected@(Selected s i) = do
  es <- findAll s
  pure ((,selected) <$> es ^? ix i)

defaultTimeout :: Timeout
defaultTimeout = Timeout 10_000

awaitElement :: (MonadIO m, WebDriver m) => Timeout -> Selector -> Runner m ActionResult
awaitElement (Timeout ms) sel@(Selector s) =
  let interval = max 100 (min 1_000 (ms `div` 10))
      loop n
        | n > ms =
          pure $ ActionFailed ("Giving up after having waited " <> show ms <> " ms for selector to match an element: " <> toS s)
        | otherwise =
          findAll sel >>= \case
            [] -> liftIO (threadDelay (fromIntegral interval)) >> loop (n + interval)
            _ -> pure ActionSuccess
   in loop 0
