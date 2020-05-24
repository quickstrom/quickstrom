{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.Gen where

import Algebra.Lattice (BoundedMeetSemiLattice (top), bottom)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import qualified Data.Text as Text
import Data.Text (Text)
import Test.QuickCheck hiding ((===), (==>))
import WTP.Element
import WTP.Specification
import WTP.Syntax hiding (map)
import WTP.Trace hiding (observedStates)
import Prelude hiding (Bool (..))

selector :: Gen Selector
selector = elements (map (Selector . Text.singleton) ['a' .. 'c'])

selected :: Gen Selected
selected = Selected <$> selector <*> choose (0, 3)

stringValues :: Gen Text
stringValues = elements ["s1", "s2", "s3"]

observedState :: Gen ObservedState
observedState = ObservedState <$> pure mempty

selectedAction :: Gen (Action Selected)
selectedAction =
  oneof
    [ Focus <$> selected,
      KeyPress <$> elements ['A' .. 'C'],
      Click <$> selected
    ]

actionResult :: Gen ActionResult
actionResult = oneof [pure ActionSuccess, pure (ActionFailed "failed"), pure ActionImpossible]

traceElement :: Gen (TraceElement ())
traceElement =
  oneof
    [ TraceAction () <$> selectedAction <*> actionResult,
      TraceState () <$> observedState
    ]

trace :: Gen (Trace ())
trace = Trace <$> listOf traceElement

nonEmpty :: Gen [a] -> Gen (NonEmpty.NonEmpty a)
nonEmpty g = fromMaybe discard . NonEmpty.nonEmpty <$> g

stringFormula :: Gen Formula
stringFormula = fromString . Text.unpack <$> stringValues

anySyntax :: Gen Formula
anySyntax = sized syntax'
  where
    syntax' :: Int -> Gen Formula
    syntax' 0 =
      oneof
        [ pure top,
          pure bottom
        ]
    syntax' n =
      let subterm = syntax' (n `div` 2)
       in oneof
            [ neg <$> subterm,
              (/\) <$> subterm <*> subterm,
              (\/) <$> subterm <*> subterm,
              (==>) <$> subterm <*> subterm,
              (<=>) <$> subterm <*> subterm,
              (===) <$> subterm <*> subterm,
              (===) <$> stringFormula <*> stringFormula,
              always <$> subterm
            ]

trueSyntax :: Gen Formula
trueSyntax = sized syntax'
  where
    syntax' :: Int -> Gen Formula
    syntax' 0 =
      oneof
        [ pure top
        ]
    syntax' n =
      let subterm = syntax' (n `div` 2)
       in oneof
            [ (/\) <$> subterm <*> subterm,
              (\/) <$> subterm <*> subterm,
              (==>) <$> subterm <*> subterm,
              (<=>) <$> subterm <*> subterm,
              always <$> subterm
            ]

falseSyntax :: Gen Formula
falseSyntax = sized syntax'
  where
    syntax' :: Int -> Gen Formula
    syntax' 0 =
      oneof [pure bottom]
    syntax' n =
      let subterm = syntax' (n `div` 2)
       in oneof
            [ (/\) <$> subterm <*> subterm,
              (\/) <$> subterm <*> subterm,
              always <$> subterm
            ]

simpleConnectivesSyntax :: Gen Formula
simpleConnectivesSyntax = sized syntax'
  where
    syntax' :: Int -> Gen Formula
    syntax' 0 =
      oneof
        [ pure top,
          pure bottom
        ]
    syntax' n =
      let subterm = syntax' (n `div` 2)
       in oneof
            [ neg <$> subterm,
              (/\) <$> subterm <*> subterm,
              (\/) <$> subterm <*> subterm
            ]
