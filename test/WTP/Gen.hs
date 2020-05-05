{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.Gen where

import Algebra.Lattice (BoundedMeetSemiLattice (top), bottom)
import qualified Data.Text as Text
import Data.Text (Text)
import Test.QuickCheck hiding ((===), (==>))
import WTP.Element
import WTP.Formula.Syntax hiding (map)
import WTP.Trace
import qualified WTP.Type as WTP
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

stringFormula :: Gen (Formula 'WTP.String)
stringFormula = fromString . Text.unpack <$> stringValues

anySyntax :: Gen (Formula 'WTP.Bool)
anySyntax = sized syntax'
  where
    syntax' :: Int -> Gen (Formula 'WTP.Bool)
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

trueSyntax :: Gen (Formula 'WTP.Bool)
trueSyntax = sized syntax'
  where
    syntax' :: Int -> Gen (Formula 'WTP.Bool)
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

falseSyntax :: Gen (Formula 'WTP.Bool)
falseSyntax = sized syntax'
  where
    syntax' :: Int -> Gen (Formula 'WTP.Bool)
    syntax' 0 =
      oneof [ pure bottom ]
    syntax' n =
      let subterm = syntax' (n `div` 2)
       in oneof
            [ (/\) <$> subterm <*> subterm,
              (\/) <$> subterm <*> subterm,
              (==>) <$> subterm <*> subterm,
              (<=>) <$> subterm <*> subterm,
              always <$> subterm
            ]

simpleConnectivesSyntax :: Gen (Formula 'WTP.Bool)
simpleConnectivesSyntax = sized syntax'
  where
    syntax' :: Int -> Gen (Formula 'WTP.Bool)
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
