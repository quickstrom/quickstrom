{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
    runActionSequence,
    reselect,
  )
where

import Control.Lens
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Loops (andM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Function ((&))
import Data.List hiding (map)
import Data.Maybe (catMaybes)
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

data Weighted a = Weighted {weight :: Int, weighted :: a}
  deriving (Show, Eq, Functor, Generic)

generateValidActions ::
  (MonadIO m, WebDriver m) =>
  Vector (Weighted (ActionSequence Selector)) ->
  Producer (ActionSequence (Element, Selected)) (Runner m) ()
generateValidActions possibleActions = loop
  where
    loop =
      lift (findActionCandidates possibleActions >>= filterValidActionCandidates >>= chooseAction)
        >>= \case
          Nothing -> pass
          Just action -> Pipes.yield action >> loop

findActionCandidates ::
  (MonadIO m, WebDriver m) =>
  Vector (Weighted (ActionSequence Selector)) ->
  Runner m (Vector (Weighted (ActionSequence (Element, Selected))))
findActionCandidates = foldMap findActionCandidate
  where
    findActionSequenceCandidate :: Weighted (ActionSequence Selector) -> Runner m (Maybe (Weighted (ActionSequence (Element, Selected))))
    findActionSequenceCandidate (Weighted weight' (Single action)) = map (Weighted weight' . Single) <$> findActionCandidate action
    findActionSequenceCandidate (Weighted weight' (Sequence actions)) = map (Weighted weight' . map Sequence) <$> traverse findActionCandidate actions

    findActionCandidate :: Action Selector -> Runner m (Maybe (Action (Element, Selected)))
    findActionCandidate = \case
      KeyPress k -> pure (pure (KeyPress k))
      EnterText t -> pure (pure (EnterText t))
      Navigate p -> pure (pure (Navigate p))
      Await sel -> pure (pure (Await sel))
      AwaitWithTimeoutSecs i sel -> pure (pure (AwaitWithTimeoutSecs i sel))
      Focus sel -> selectOne sel Focus (if skipValidation then alwaysTrue else isNotActive)
      Click sel -> selectOne sel Click (if skipValidation then alwaysTrue else isClickable)
      Clear sel -> selectOne sel Clear (if skipValidation then alwaysTrue else isClearable)
      Refresh -> pure (pure Refresh)

filterValidActionCandidates ::
  (MonadIO m, WebDriver m) =>
  Vector (Weighted (ActionSequence (Element, Selected))) ->
  Runner m (Vector (Weighted (ActionSequence (Element, Selected))))
filterValidActionCandidates = _

chooseAction ::
  (MonadIO m, WebDriver m) =>
  Vector (Weighted (ActionSequence (Element, Selected))) ->
  Runner m (Maybe (ActionSequence (Element, Selected)))
chooseAction = _

-- actions'
--   & QuickCheck.frequency
--   & generate

selectValidActionSeq :: (MonadIO m, WebDriver m) => ActionSequence Selector -> Runner m (Maybe (ActionSequence Selected))
selectValidActionSeq (Single action) = map Single <$> selectValidAction False action
selectValidActionSeq (Sequence (a :| as)) =
  selectValidAction False a >>= \case
    Just firstAction -> do
      restActions <- traverse (selectValidAction True) as
      pure (Just (Sequence (firstAction :| catMaybes restActions)))
    Nothing -> pure Nothing

selectValidAction ::
  (MonadIO m, WebDriver m) => Bool -> Action Selector -> Runner m (Maybe (Action Selected))
selectValidAction skipValidation possibleAction =
  case possibleAction of
    KeyPress k -> do
      active <- isActiveInput
      if skipValidation || active then pure (Just (KeyPress k)) else pure Nothing
    EnterText t -> do
      active <- isActiveInput
      if skipValidation || active then pure (Just (EnterText t)) else pure Nothing
    Navigate p -> pure (Just (Navigate p))
    Await sel -> pure (Just (Await sel))
    AwaitWithTimeoutSecs i sel -> pure (Just (AwaitWithTimeoutSecs i sel))
    Focus sel -> selectOne sel Focus (if skipValidation then alwaysTrue else isNotActive)
    Click sel -> selectOne sel Click (if skipValidation then alwaysTrue else isClickable)
    Clear sel -> selectOne sel Clear (if skipValidation then alwaysTrue else isClearable)
    Refresh -> pure (Just Refresh)
  where
    selectOne ::
      (MonadIO m, WebDriver m) =>
      Selector ->
      (Selected -> Action Selected) ->
      (Element -> m Bool) ->
      m (Maybe (Action Selected))
    selectOne sel ctor isValid = do
      found <- findAll sel
      validChoices <-
        filterM
          (\(_, e) -> isValid e `catchResponseError` const (pure False))
          (zip [0 ..] found)
      case validChoices of
        [] -> pure Nothing
        choices -> Just <$> generate (ctor . Selected sel <$> QuickCheck.elements (map fst choices))
    isNotActive e = (/= Just e) <$> activeElement
    activeElement = (Just <$> getActiveElement) `catchResponseError` const (pure Nothing)
    alwaysTrue = const (pure True)
    isClickable e = do
      scripts <- asks checkScripts
      andM [isElementEnabled e, runCheckScript (isElementVisible scripts e)]
    isClearable el = (`elem` ["input", "textarea"]) <$> getElementTagName el
    isActiveInput =
      activeElement >>= \case
        Just el -> (`elem` ["input", "textarea"]) <$> getElementTagName el
        Nothing -> pure False

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
  pure ((,selected) <$> (es ^? ix i))

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
