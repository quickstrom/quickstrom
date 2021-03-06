{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Quickstrom.CLI.Reporter.JSONTest where

import Control.Lens hiding (elements)
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.TreeDiff.Class (ToExpr (..))
import Data.TreeDiff.QuickCheck (ediffEq)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Quickstrom.Action as Quickstrom
import Quickstrom.CLI.Reporter.JSON hiding (elements, toActionSubject)
import qualified Quickstrom.Element as Quickstrom
import Quickstrom.Prelude hiding (State)
import qualified Quickstrom.Trace as Quickstrom
import Test.QuickCheck hiding (vectorOf)
import qualified Test.QuickCheck.Gen as Gen
import Test.Tasty.Hspec hiding (Selector)

spec_htmlReporter :: Spec
spec_htmlReporter =
  it "parses the trace as transitions" $
    property $ \(TestTransitions expected) ->
      let trace' = toTrace expected
          actual = traceToTransitions trace'
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

toTrace :: Vector (Transition ByteString) -> Quickstrom.Trace Quickstrom.TraceElementEffect
toTrace ts =
  case ts ^? _Cons of
    Nothing -> Quickstrom.Trace []
    Just (x, _) ->
      Quickstrom.Trace
        ( pure (toStateElement (x ^. #states . #from))
            <> foldMap toTransition ts
        )

toTransition :: Transition ByteString -> [Quickstrom.TraceElement Quickstrom.TraceElementEffect]
toTransition (Transition action' (States _ to') _) =
  maybe [] (\(a :| as) -> pure (toActionElement (Quickstrom.ActionSequence a as))) action'
    <> [toStateElement to']

toActionElement :: Quickstrom.ActionSequence ActionSubject ActionSubject -> Quickstrom.TraceElement Quickstrom.TraceElementEffect
toActionElement a = Quickstrom.TraceAction Quickstrom.NoStutter (bimap toActionSubject toActionSubject a) Quickstrom.ActionSuccess

toActionSubject :: ActionSubject -> Quickstrom.ActionSubject
toActionSubject as =
  Quickstrom.ActionSubject
    (as ^. #selected)
    (Quickstrom.Element (as ^. #element . #id))
    (as ^. #element . #position)

-- toActionSubject :: Quickstrom.ActionSubject -> ActionSubject
-- toActionSubject as =
--   ActionSubject
--     { selected = as ^. #selected,
--       element = ActionElement {id = as ^. #element . #ref, position = as ^. #position}
--     }

toStateElement :: State ByteString -> Quickstrom.TraceElement Quickstrom.TraceElementEffect
toStateElement (State screenshot' queries') =
  Quickstrom.TraceState
    Quickstrom.NoStutter
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

toObservedElementState :: QueriedElement -> Quickstrom.ObservedElementState
toObservedElementState element' =
  Quickstrom.ObservedElementState
    (Quickstrom.Element (element' ^. #id))
    (element' ^. #position)
    (HashMap.fromList [toPair s | s <- Vector.toList (element' ^. #state)])
  where
    toPair (ElementStateValue state' value' _) = (state', value')

newtype TestTransitions = TestTransitions (Vector (Transition ByteString))
  deriving (Eq, Show)

instance Arbitrary TestTransitions where
  arbitrary = TestTransitions <$> genTransitions
  shrink (TestTransitions txs) =
    map
      (TestTransitions . Vector.fromList)
      (shrinkList shrinkNothing (Vector.toList txs))

genTransitions :: Gen (Vector (Transition ByteString))
genTransitions = do
  n <- getPositive <$> arbitrary
  t1 <- genTransitionFrom =<< genState
  Vector.iterateNM n (genTransitionFrom . view (#states . #to)) t1

genTransitionFrom :: State ByteString -> Gen (Transition ByteString)
genTransitionFrom from' =
  Transition
    <$> Gen.oneof [Just . Quickstrom.actionSequenceToNonEmpty <$> genActionSequence, pure Nothing]
    <*> (States from' <$> genState)
    <*> pure False

genState :: Gen (State ByteString)
genState = State Nothing <$> vectorOf genQuery `suchThat` (not . hasDuplicates . map (view #selector))

genQuery :: Gen Query
genQuery = Query <$> identifier "selector-" <*> vectorOf genQueriedElement `suchThat` (not . hasDuplicates . map (view #id))

genQueriedElement :: Gen QueriedElement
genQueriedElement = QueriedElement <$> identifier "element-" <*> (Just <$> genPosition) <*> pure []

genPosition :: Gen Quickstrom.Position
genPosition = Quickstrom.Position <$> genNat <*> genNat <*> genNat <*> genNat

genNat :: Gen Int
genNat = getPositive <$> arbitrary

genActionSequence :: Gen (Quickstrom.ActionSequence ActionSubject ActionSubject)
genActionSequence = pure (Quickstrom.ActionSequence (Quickstrom.KeyPress 'a') [])

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

instance ToExpr QueriedElement

instance ToExpr ActionSubject

instance ToExpr Quickstrom.Element

instance ToExpr ActionElement

instance ToExpr Diff

instance ToExpr Quickstrom.ElementState

instance ToExpr ElementStateValue

instance (ToExpr s1, ToExpr s2) => ToExpr (Quickstrom.ActionSequence s1 s2)

instance ToExpr s => ToExpr (Quickstrom.Action s)

instance ToExpr Quickstrom.Selector

instance ToExpr Quickstrom.Selected
