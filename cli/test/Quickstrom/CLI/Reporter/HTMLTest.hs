{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Quickstrom.CLI.Reporter.HTMLTest where

import Control.Lens hiding (elements)
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.TreeDiff.Class (ToExpr (..))
import Data.TreeDiff.QuickCheck (ediffEq)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Quickstrom.CLI.Reporter.HTML hiding (elements)
import qualified Quickstrom.Element as Quickstrom
import Quickstrom.Prelude hiding (State)
import qualified Quickstrom.Trace as Quickstrom
import Test.QuickCheck hiding (vectorOf)
import qualified Test.QuickCheck.Gen as Gen
import Test.Tasty.Hspec hiding (Selector)

spec_htmlReporter :: Spec
spec_htmlReporter =
  it "parses the trace as transitions" $
    property $ \(Transitions expected) ->
      let trace' = toTrace expected
          actual = traceToTransition trace'
       in sortTransitions actual `ediffEq` sortTransitions expected

sortTransitions :: Vector (Transition ByteString) -> Vector (Transition ByteString)
sortTransitions xs =
  xs
    & traversed . #states . #from . #queries %~ sortQueries
    & traversed . #states . #to . #queries %~ sortQueries
  where
    sortQueries =
      map (#elements %~ vsortOn (view #id))
        . vsortOn (view #selector)

vsortOn :: Ord o => (a -> o) -> Vector a -> Vector a
vsortOn sel =
  Vector.fromList . sortOn sel . Vector.toList

toTrace :: Vector (Transition ByteString) -> Quickstrom.Trace ()
toTrace ts =
  case ts ^? _Cons of
    Nothing -> Quickstrom.Trace []
    Just (x, _) ->
      Quickstrom.Trace
        ( pure (toStateElement (x ^. #states . #from))
            <> (foldMap toTransition ts)
        )

toTransition :: Transition ByteString -> [Quickstrom.TraceElement ()]
toTransition (Transition action' (States _ to')) =
  maybe [] (pure . toActionElement) action'
    <> [toStateElement to']

toActionElement :: Quickstrom.Action Quickstrom.Selected -> Quickstrom.TraceElement ()
toActionElement a = Quickstrom.TraceAction () a Quickstrom.ActionSuccess

toStateElement :: State ByteString -> Quickstrom.TraceElement ()
toStateElement (State screenshot' queries') =
  Quickstrom.TraceState
    ()
    ( Quickstrom.ObservedState
        screenshot'
        ( Quickstrom.ObservedElementStates
            ( HashMap.fromList
                [ ( Quickstrom.Selector (query ^. #selector),
                    map toObservedElementState (Vector.toList (query ^. #elements))
                  )
                  | query <- Vector.toList queries'
                ]
            )
        )
    )

toObservedElementState :: Element -> Quickstrom.ObservedElementState
toObservedElementState element' =
  Quickstrom.ObservedElementState
    (Quickstrom.Element (element' ^. #id))
    (element' ^. #position)
    (HashMap.fromList [toPair s | s <- Vector.toList (element' ^. #state)])
  where
    toPair = \case
      Attribute name' value' -> (Quickstrom.Attribute name', value')
      Property name' value' -> (Quickstrom.Property name', value')
      CssValue name' value' -> (Quickstrom.CssValue name', value')
      Text value' -> (Quickstrom.Text, value')

newtype Transitions = Transitions (Vector (Transition ByteString))
  deriving (Eq, Show)

instance Arbitrary Transitions where
  arbitrary = Transitions <$> genTransitions
  shrink (Transitions txs) =
    map
      (Transitions . Vector.fromList)
      (shrinkList shrinkNothing (Vector.toList txs))

genTransitions :: Gen (Vector (Transition ByteString))
genTransitions = do
  n <- getPositive <$> arbitrary
  t1 <- genTransitionFrom =<< genState
  Vector.iterateNM n (genTransitionFrom . view (#states . #to)) t1

genTransitionFrom :: State ByteString -> Gen (Transition ByteString)
genTransitionFrom from' =
  Transition
    <$> Gen.oneof [Just <$> genAction, pure Nothing]
    <*> (States from' <$> genState)

genState :: Gen (State ByteString)
genState = State Nothing <$> vectorOf genQuery `suchThat` (not . hasDuplicates . map (view #selector))

genQuery :: Gen Query
genQuery = Query <$> identifier "selector-" <*> vectorOf genElement `suchThat` (not . hasDuplicates . map (view #id))

genElement :: Gen Element
genElement = Element <$> identifier "element-" <*> pure Modified <*> genPosition <*> pure []

genPosition :: Gen Quickstrom.Position
genPosition = Quickstrom.Position <$> genNat <*> genNat <*> genNat <*> genNat

genNat :: Gen Int
genNat = getPositive <$> arbitrary

genAction :: Gen (Quickstrom.Action Quickstrom.Selected)
genAction = pure (Quickstrom.KeyPress 'a')

identifier :: [Char] -> Gen Text
identifier prefix = Text.pack . (prefix <>) . show <$> arbitrary @Word

bytestring :: Gen ByteString
bytestring = toS <$> arbitrary @[Char]

vectorOf :: Gen a -> Gen (Vector a)
vectorOf g = Vector.fromList <$> Gen.listOf g

hasDuplicates :: (Eq a, Hashable a) => Vector a -> Bool
hasDuplicates xs =
  length (HashSet.fromList (Vector.toList xs)) /= length xs

instance ToExpr screenshot => ToExpr (Transition screenshot)

instance ToExpr screenshot => ToExpr (States screenshot)

instance ToExpr screenshot => ToExpr (State screenshot)

instance ToExpr Query

instance ToExpr Quickstrom.Position

instance ToExpr Element

instance ToExpr Status

instance ToExpr ElementState

instance ToExpr s => ToExpr (Quickstrom.Action s)

instance ToExpr Quickstrom.Selector

instance ToExpr Quickstrom.Selected