{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Quickstrom.Gen where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Quickstrom.Action
import Quickstrom.Element
import Quickstrom.Trace hiding (observedStates)
import Test.QuickCheck hiding ((===), (==>))
import Prelude hiding (Bool (..))

selector :: Gen Selector
selector = elements (map (Selector . Text.singleton) ['a' .. 'c'])

selected :: Gen Selected
selected = Selected <$> selector <*> choose (0, 3)

stringValues :: Gen Text
stringValues = elements ["s1", "s2", "s3"]

observedState :: Gen ObservedState
observedState = pure mempty

selectedAction :: Gen (Action Selected)
selectedAction =
  oneof
    [ Focus <$> selected,
      KeyPress <$> elements ['A' .. 'C'],
      Click <$> selected
    ]

selectedActionSequence :: Gen (ActionSequence Selected)
selectedActionSequence = ActionSequence . pure <$> selectedAction

actionResult :: Gen ActionResult
actionResult = oneof [pure ActionSuccess, pure (ActionFailed "failed"), pure ActionImpossible]

traceElementWithState :: Gen ObservedState -> Gen (TraceElement ())
traceElementWithState genState =
  oneof
    [ TraceAction () <$> selectedActionSequence <*> actionResult,
      TraceState () <$> genState
    ]

traceElement :: Gen (TraceElement ())
traceElement = traceElementWithState observedState

traceWithState :: Gen ObservedState -> Gen (Trace ())
traceWithState genState = do
  s0 <- genState
  Trace . (TraceState () s0 :) <$> listOf (traceElementWithState genState)

trace :: Gen (Trace ())
trace = traceWithState observedState

nonEmpty :: Gen [a] -> Gen (NonEmpty.NonEmpty a)
nonEmpty g = fromMaybe discard . NonEmpty.nonEmpty <$> g
