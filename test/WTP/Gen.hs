{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.Gen where

import Algebra.Lattice (BoundedMeetSemiLattice (top), bottom)
import qualified Data.Text as Text
import Data.Text (Text)
import Test.QuickCheck hiding ((===), (==>))
import WTP.Element
import WTP.Formula.Syntax
import WTP.Trace
import Prelude hiding (Bool (..))
import Data.String (IsString(fromString))

selector :: Gen Selector
selector = elements (map (Selector . Text.singleton) ['a' .. 'c'])

stringValues :: Gen Text
stringValues = elements ["s1", "s2", "s3"]

observedState :: Gen ObservedState
observedState = ObservedState <$> pure mempty <*> pure mempty -- TODO

trace :: Gen [ObservedState]
trace = listOf observedState

nonEmpty :: Gen [a] -> Gen [a]
nonEmpty = flip suchThat (Prelude.not . null)

stringFormula :: Gen (Formula Text)
stringFormula = fromString . Text.unpack <$> stringValues

anySyntax :: Gen Proposition
anySyntax = sized syntax'
  where
    syntax' :: Int -> Gen Proposition
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

trueSyntax :: Gen Proposition
trueSyntax = sized syntax'
  where
    syntax' :: Int -> Gen Proposition
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

falseSyntax :: Gen Proposition
falseSyntax = sized syntax'
  where
    syntax' :: Int -> Gen Proposition
    syntax' 0 =
      oneof [ pure bottom ]
    syntax' n =
      let subterm = syntax' (n `div` 2)
       in oneof
            [ (/\) <$> subterm <*> subterm,
              (\/) <$> subterm <*> subterm,
              always <$> subterm
            ]

simpleConnectivesSyntax :: Gen Proposition
simpleConnectivesSyntax = sized syntax'
  where
    syntax' :: Int -> Gen Proposition
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
