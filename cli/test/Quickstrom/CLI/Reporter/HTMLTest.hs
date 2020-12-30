{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Quickstrom.CLI.Reporter.HTMLTest where

import Control.Lens hiding (elements)
import qualified Data.Aeson as JSON
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
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
  it "parses the trace as transitions" $ property $ \(Transactions expected) ->
      let trace' = toTrace expected
          actual = traceToTransition trace'
       in expected `ediffEq` actual

toTrace :: Vector (Transition ByteString) -> Quickstrom.Trace ()
toTrace ts = Quickstrom.Trace (foldMap toTransition ts)

toTransition :: Transition ByteString -> [Quickstrom.TraceElement ()]
toTransition (Transition action' (States from' to')) =
  [toStateElement from']
    <> maybe [] (pure . toActionElement) action'
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
                    map toStateMap (Vector.toList (query ^. #elements))
                  )
                  | query <- Vector.toList queries'
                ]
            )
        )
    )

toStateMap :: Element -> HashMap Quickstrom.ElementState JSON.Value
toStateMap element' =
  HashMap.fromList [toPair s | s <- Vector.toList (element' ^. #state)]
  where
    toPair = \case
      Attribute name' value' -> (Quickstrom.Attribute name', value')
      Property name' value' -> (Quickstrom.Property name', value')
      CssValue name' value' -> (Quickstrom.CssValue name', value')
      Text value' -> (Quickstrom.Text, value')

newtype Transactions = Transactions (Vector (Transition ByteString))
  deriving (Eq, Show)

instance Arbitrary Transactions where
  arbitrary = Transactions <$> vectorOf genTransition
  shrink (Transactions txs) =
    map
      (Transactions . Vector.fromList)
      (shrinkList shrinkNothing (Vector.toList txs))

genTransitions :: Gen (Vector (Transition ByteString))
genTransitions = vectorOf genTransition

genTransition :: Gen (Transition ByteString)
genTransition =
  Transition
    <$> Gen.oneof [Just <$> genAction, pure Nothing]
    <*> (States <$> genState <*> genState)

genState :: Gen (State ByteString)
genState = State Nothing <$> vectorOf genQuery `suchThat` (not . hasDuplicates . map (view #selector))

genQuery :: Gen Query
genQuery = Query <$> text <*> vectorOf genElement

genElement :: Gen Element
genElement = Element <$> text <*> pure Modified <*> pure []

genAction :: Gen (Quickstrom.Action Quickstrom.Selected)
genAction = pure (Quickstrom.KeyPress 'a')

text :: Gen Text
text = Text.singleton <$> elements ['a' .. 'e']

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

instance ToExpr Element

instance ToExpr Status

instance ToExpr ElementState

instance ToExpr s => ToExpr (Quickstrom.Action s)

instance ToExpr Quickstrom.Selector

instance ToExpr Quickstrom.Selected