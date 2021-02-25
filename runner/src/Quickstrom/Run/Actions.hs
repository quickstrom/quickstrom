{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Quickstrom.Run.Actions
  ( awaitElement,
    defaultAwaitSecs,
    shrinkAction,
    generateValidActions,
    runActionSequence,
  )
where

import Control.Lens hiding (each)
import Control.Monad (filterM, (>=>))
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
import Quickstrom.Run.Scripts (CheckScripts (..), runCheckScript)
import Quickstrom.Element
import Quickstrom.Prelude hiding (catch, check, trace)
import Quickstrom.Trace
import Quickstrom.WebDriver.Class
import qualified Test.QuickCheck as QuickCheck
import Quickstrom.Run.Runner

shrinkAction :: ActionSequence Selected -> [ActionSequence Selected]
shrinkAction _ = [] -- TODO?

generate :: MonadIO m => QuickCheck.Gen a -> m a
generate = liftIO . QuickCheck.generate

generateValidActions :: (MonadIO m, WebDriver m) => Vector (Int, ActionSequence Selector) -> Producer (ActionSequence Selected) (Runner m) ()
generateValidActions possibleActions = loop
  where
    loop = do
      validActions <- lift $ for (Vector.toList possibleActions) \(prob, action') -> do
        fmap (prob,) <$> selectValidActionSeq action'
      case map (_2 %~ pure) (catMaybes validActions) of
        [] -> pass
        actions' -> do
          actions'
            & QuickCheck.frequency
            & generate
            & lift
            & (>>= Pipes.yield)
          loop

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

click :: WebDriver m => Selected -> Runner m ActionResult
click =
  findSelected >=> \case
    Just e -> tryAction (ActionSuccess <$ elementClick e)
    Nothing -> pure ActionImpossible

clear :: WebDriver m => Selected -> Runner m ActionResult
clear =
  findSelected >=> \case
    Just e -> tryAction (ActionSuccess <$ elementClear e)
    Nothing -> pure ActionImpossible

sendKeys :: WebDriver m => Text -> Runner m ActionResult
sendKeys t = tryAction (ActionSuccess <$ (getActiveElement >>= elementSendKeys t))

sendKey :: WebDriver m => Char -> Runner m ActionResult
sendKey = sendKeys . Text.singleton

focus :: WebDriver m => Selected -> Runner m ActionResult
focus =
  findSelected >=> \case
    Just e -> tryAction (ActionSuccess <$ elementSendKeys "" e)
    Nothing -> pure ActionImpossible

runAction :: (MonadIO m, WebDriver m) => Action Selected -> Runner m ActionResult
runAction = \case
  Focus s -> focus s
  KeyPress c -> sendKey c
  EnterText t -> sendKeys t
  Click s -> click s
  Clear s -> clear s
  Await s -> awaitElement defaultAwaitSecs s
  AwaitWithTimeoutSecs i s -> awaitElement i s
  Navigate uri -> tryAction (ActionSuccess <$ navigateTo uri)
  Refresh -> tryAction (ActionSuccess <$ pageRefresh)

runActionSequence :: (MonadIO m, WebDriver m) => ActionSequence Selected -> Runner m ActionResult
runActionSequence = \case
  Single h -> runAction h
  Sequence actions' ->
    let loop [] = pure ActionSuccess
        loop (x : xs) =
          runAction x >>= \case
            ActionSuccess -> loop xs
            err -> pure err
     in loop (toList actions')

findSelected :: WebDriver m => Selected -> m (Maybe Element)
findSelected (Selected s i) =
  findAll s >>= \es -> pure (es ^? ix i)

defaultAwaitSecs :: Int
defaultAwaitSecs = 10

-- TODO: Rewrite to use Timeout
awaitElement :: (MonadIO m, WebDriver m) => Int -> Selector -> Runner m ActionResult
awaitElement secondsTimeout sel@(Selector s) =
  let loop n
        | n > secondsTimeout =
          pure $ ActionFailed ("Giving up after having waited " <> show secondsTimeout <> " seconds for selector to match an element: " <> toS s)
        | otherwise =
          findAll sel >>= \case
            [] -> liftIO (threadDelay 1000000) >> loop (n + 1)
            _ -> pure ActionSuccess
   in loop (1 :: Int)
