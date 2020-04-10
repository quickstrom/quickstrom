{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Formula.Syntax
  ( Assertion (..),
    Query.Selector (..),
    Query.ElementState (..),
    Query.Element (..),
    Query.Query,
    Query.query,
    Query.queryAll,
    Query.get,
    Formula (..),
    simplify,
    toNNF,
    (/\),
    (\/),
    (∧),
    (∨),
    (===),
    (≡),
    (¬),
    not,
    (⊢),
  )
where

-- require,

import Control.Monad.Freer
import Data.Bool (Bool)
import WTP.Assertion
import qualified WTP.Formula.Minimal as Minimal
import qualified WTP.Formula.NNF as NNF
import WTP.Query as Query
import Prelude hiding
  ( Bool (..),
    not,
  )

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
  -- Derived operators (only present in syntax) are simplified
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
  
toNNF :: Formula -> NNF.Formula
toNNF = \case
  -- Negation propagation (https://en.wikipedia.org/wiki/Linear_temporal_logic#Negation_normal_form)
  Not True -> NNF.False
  Not False -> NNF.True
  Not (p `Or` q) -> toNNF p `NNF.And` toNNF q
  Not (p `And` q) -> toNNF (Not p) `NNF.Or` toNNF (Not q)
  Not (Until p q) -> toNNF (Not p) `NNF.Release` toNNF (Not q)
  Not (Release p q) -> toNNF (Not p) `NNF.Until` toNNF (Not q)
  Not p -> toNNF (Not p) `NNF.Until` toNNF (Not q)

  -- Derived operators (only present in syntax) are simplified
  Eventually p -> NNF.Until NNF.True (toNNF p)
  Always p -> toNNF (Not (Eventually (Not p)))
  Implies p q -> toNNF (Not p `Or` q)
  Equivalent p q -> toNNF (p `Implies` q) `NNF.Or` toNNF (q `Implies` p)
  -- Language operators that are preserved
  True -> NNF.True
  False -> NNF.False
  Or p q -> NNF.Or (toNNF p) (toNNF q)
  And p q -> NNF.And (toNNF p) (toNNF q)
  Until p q -> NNF.Until (toNNF p) (toNNF q)
  Release p q -> NNF.Release (toNNF p) (toNNF q)
  Assert query assertion -> NNF.Assert (NNF.Pos (NNF.QueryAssertion query assertion))


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
