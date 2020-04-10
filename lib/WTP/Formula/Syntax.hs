{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module WTP.Formula.Syntax
  ( Assertion (..)
  , Query.Selector (..)
  , Query.ElementState (..)
  , Query.Element (..)
  , Query.Query
  , Query.query
  , Query.queryAll
  , Query.get
  , Formula (..),
    simplify,
    (/\),
    (\/),
    (∧),
    (∨),
    (===),
    (≡),
    (¬),
    not,
    (⊢),
    -- require,
  )
where

import           Control.Monad.Freer
import           Data.Bool                 (Bool)
import           Prelude                 hiding ( Bool(..)
                                                , not
                                                )
import qualified WTP.Formula.Minimal                    as Minimal
import           WTP.Assertion
import           WTP.Query                 as Query

data Formula where
  -- Simplified language operators
  True :: Formula
  Not :: Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Until :: Formula -> Formula -> Formula
  Assert :: Show a => Eff '[Query] a -> Assertion a -> Formula
  -- Full language operators
  False :: Formula
  Eventually :: Formula -> Formula
  Always :: Formula -> Formula
  And :: Formula -> Formula -> Formula
  Implies :: Formula -> Formula -> Formula
  Equivalent :: Formula -> Formula -> Formula
  Release :: Formula -> Formula -> Formula

simplify :: Formula -> Minimal.Formula
simplify = \case
  -- Derived operators (only present in `Full` language) are simplified
  False -> Minimal.Not Minimal.True
  Eventually p -> Minimal.Until Minimal.True (simplify p)
  Always p -> Minimal.Not (simplify (Eventually (Not p)))
  And p q -> Minimal.Not (Minimal.Not (simplify p) `Minimal.Or` Minimal.Not (simplify q))
  Implies p q -> Minimal.Not (simplify p) `Minimal.Or` simplify q
  Equivalent p q -> simplify (p `Implies` q) `Minimal.Or` simplify (q `Implies` p)
  Release p q -> Minimal.Not (Minimal.Not (simplify p) `Minimal.Until` Minimal.Not (simplify q))
  -- Simplified language operators are preserved
  True -> Minimal.True
  Not p -> Minimal.Not (simplify p)
  Or p q -> Minimal.Or (simplify p) (simplify q)
  Until p q -> Minimal.Until (simplify p) (simplify q)
  Assert query assertion -> Minimal.Assert query assertion

infix 4 \/, /\, ∧, ∨

(/\), (\/), (∧), (∨) :: Formula -> Formula -> Formula
(/\) = And
(\/) = Or
(∧) = And
(∨) = Or

infix 5 ===, ≡

(===), (≡) :: (Show a, Eq a) => Eff '[Query] a -> a -> Formula
query' === expected = Assert query' (Equals expected)
(≡) = (===)

(⊢) :: Show a => Eff '[Query] a -> (a -> Bool) -> Formula
query' ⊢ f = Assert query' (Satisfies f)

infix 6 ¬

not, (¬) :: Formula -> Formula
not = Not
(¬) = Not
