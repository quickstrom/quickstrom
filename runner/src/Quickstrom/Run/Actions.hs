{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Quickstrom.Run.Actions
  ( awaitElement,
    defaultTimeout,
    shrinkAction,
    generateValidActions,
    isCurrentlyValid,
    isActionCurrentlyValid,
    runActionSequence,
    Selectable,
    selectOne,
    chooseAction,
    recordActionSubjectPositions,
  )
where

import Control.Lens
import Data.Generics.Labels ()
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
import Quickstrom.Trace (ActionResult (..), ActionSubject(..))
import Quickstrom.WebDriver.Class
import qualified Test.QuickCheck as QuickCheck
import Control.Monad.Catch (MonadCatch(catch))
import Control.Monad.Trans.Writer.Strict (runWriterT, tell, WriterT)

shrinkAction :: ActionSequence Selector Selected -> [ActionSequence Selector Selected]
shrinkAction _ = [] -- TODO?

generate :: MonadIO m => QuickCheck.Gen a -> m a
generate = liftIO . QuickCheck.generate

generateValidActions ::
  (MonadIO m, MonadCatch m, WebDriver m) =>
  Vector (Weighted (ActionSequence Selector Selector)) ->
  Producer (ActionSequence Selector ActionSubject) (Runner m) ()
generateValidActions possibleActions = loop
  where
    loop =
      lift (findActionCandidates possibleActions >>= filterCurrentlyValidActionCandidates >>= chooseAction)
        >>= \case
          Nothing -> pass
          Just action -> lift (recordActionSubjectPositions action) >>= Pipes.yield >> loop

findActionCandidates ::
  (MonadIO m, WebDriver m) =>
  Vector (Weighted (ActionSequence Selector Selector)) ->
  Runner m (Vector (Weighted (ActionSequence Selector ActionSubject)))
findActionCandidates = map fold . traverse (map (maybe mempty pure) . findActionSequenceCandidate)
  where
    findActionSequenceCandidate :: (MonadIO m, WebDriver m) => Weighted (ActionSequence Selector Selector) -> Runner m (Maybe (Weighted (ActionSequence Selector ActionSubject)))
    findActionSequenceCandidate (Weighted weight' (ActionSequence action actions)) =
      findActionCandidate action >>= \case
        Just candidate -> pure (pure (Weighted weight' (ActionSequence candidate actions)))
        Nothing -> pure Nothing

    findActionCandidate :: (MonadIO m, WebDriver m) => Action Selector -> Runner m (Maybe (Action ActionSubject))
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

    findCandidate :: (MonadIO m, WebDriver m) => Selector -> Runner m (Maybe ActionSubject)
    findCandidate sel = do
      found <- findAll sel
      case zip [0 ..] found of
        [] -> pure Nothing
        choices -> Just <$> generate (QuickCheck.elements [ActionSubject (Selected sel i) e Nothing | (i, e) <- choices])

filterCurrentlyValidActionCandidates ::
  (MonadIO m, MonadCatch m, WebDriver m) =>
  Vector (Weighted (ActionSequence Selector ActionSubject)) ->
  Runner m (Vector (Weighted (ActionSequence Selector ActionSubject)))
filterCurrentlyValidActionCandidates = Vector.filterM (isCurrentlyValid . weighted)

isCurrentlyValid :: (MonadCatch m, WebDriver m) => ActionSequence s ActionSubject -> Runner m Bool
isCurrentlyValid (ActionSequence action _) = isActionCurrentlyValid action

isActionCurrentlyValid :: (MonadCatch m, WebDriver m) => Action ActionSubject -> Runner m Bool
isActionCurrentlyValid = \case
  KeyPress _ -> isActiveInput
  EnterText _ -> isActiveInput
  Navigate _ -> pure True
  Await _el -> pure True
  AwaitWithTimeoutSecs _ _ -> pure True
  Focus (ActionSubject _ el _) -> isNotActive el
  Click (ActionSubject _ el _) -> isClickable el
  Clear (ActionSubject _ el _) -> isClearable el
  Refresh -> pure True
  where
    isNotActive e = (/= Just e) <$> activeElement
    activeElement = (Just <$> getActiveElement) `catch` (\WebDriverResponseError{} -> pure Nothing)
    isClickable e = do
      scripts <- asks checkScripts
      getElementTagName e >>= \case
        "option" -> isElementEnabled e
        _ -> andM [isElementEnabled e, runCheckScript (isElementVisible scripts e)]
    isClearable el = (`elem` ["input", "textarea"]) <$> getElementTagName el
    isActiveInput =
      activeElement >>= \case
        Just el -> (`elem` ["input", "textarea"]) <$> getElementTagName el
        Nothing -> pure False

chooseAction ::
  (MonadIO m) =>
  Vector (Weighted (ActionSequence Selector ActionSubject)) ->
  Runner m (Maybe (ActionSequence Selector ActionSubject))
chooseAction choices
  | Vector.null choices = pure Nothing
  | otherwise = Just <$> generate (QuickCheck.frequency [(w, pure x) | Weighted w x <- Vector.toList choices])

recordActionSubjectPositions ::
  (MonadIO m, WebDriver m) =>
  ActionSequence Selector ActionSubject
  -> Runner m (ActionSequence Selector ActionSubject)
recordActionSubjectPositions seqs = do
  scripts <- asks checkScripts
  for seqs (\subject -> do
      p <- runCheckScript (getPosition scripts (subject ^. #element))
      pure (subject { position = p })
    )

tryAction :: MonadCatch m => Runner m ActionResult -> Runner m ActionResult
tryAction action =
  action `catch` (\(WebDriverResponseError msg) -> pure (ActionFailed msg))

click :: (MonadCatch m, WebDriver m) => Element -> Runner m ActionResult
click e = tryAction (ActionSuccess <$ elementClick e)

clear :: (MonadCatch m, WebDriver m) => Element -> Runner m ActionResult
clear e = tryAction (ActionSuccess <$ elementClear e)

sendKeys :: (MonadCatch m, WebDriver m) => Text -> Runner m ActionResult
sendKeys t = tryAction (ActionSuccess <$ (getActiveElement >>= elementSendKeys t))

sendKey :: (MonadCatch m, WebDriver m) => Char -> Runner m ActionResult
sendKey = sendKeys . Text.singleton

focus :: (MonadCatch m, WebDriver m) => Element -> Runner m ActionResult
focus e = tryAction (ActionSuccess <$ elementSendKeys "" e)

runAction :: (MonadIO m, MonadCatch m, WebDriver m) => Action Element -> Runner m ActionResult
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

runActionSequence :: (MonadIO m, MonadCatch m, WebDriver m, Selectable s) => ActionSequence s ActionSubject -> Runner m (ActionSequence ActionSubject ActionSubject, ActionResult)
runActionSequence (ActionSequence action actions) = do
  (result, actions') <- runWriterT (ifSuccess (lift (runAction (action & traverse %~ view #element))) (runRest actions))
  pure (ActionSequence action actions', result)
  where
    ifSuccess :: Monad m => m ActionResult -> m ActionResult -> m ActionResult
    ifSuccess a b = 
        a >>= \case
          ActionSuccess -> b
          err -> pure err
    runRest :: (WebDriver m, MonadCatch m, MonadIO m, Selectable s) => [Action s] -> WriterT [Action ActionSubject] (Runner m) ActionResult
    runRest [] = pure ActionSuccess
    runRest (x : xs) = do
      x' <- traverse (lift . selectOne) x
      ifSuccess (tell [x'] >> lift (runAction (x' & traverse %~ view #element))) (runRest xs)

class Selectable s where
  selectOne :: WebDriver m => s -> m ActionSubject

instance Selectable Selector where
  selectOne s = do
    es <- findAll s
    case es ^? ix 0 of
      Just e -> pure (ActionSubject (Selected s 0) e Nothing)
      Nothing -> fail ("Couldn't select a single element with selector: " <> show s)

instance Selectable Selected where
  selectOne selected'@(Selected s i) = do
    es <- findAll s
    case es ^? ix i of
      Just e -> pure (ActionSubject selected' e Nothing)
      Nothing -> fail ("Couldn't select a single element with selector: " <> show s)

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
            [] -> liftIO (threadDelay (fromIntegral interval * 1000)) >> loop (n + interval)
            _ -> pure ActionSuccess
   in loop 0
