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
    FormulaWith (..),
    Formula,
    depth,
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

data FormulaWith assertion
  = -- Simplified language operators
    True
  | Not (FormulaWith assertion)
  | Or (FormulaWith assertion) (FormulaWith assertion)
  | Until (FormulaWith assertion) (FormulaWith assertion)
  | Assert assertion
  | -- Full language operators
    False
  | Eventually (FormulaWith assertion)
  | Always (FormulaWith assertion)
  | And (FormulaWith assertion) (FormulaWith assertion)
  | Implies (FormulaWith assertion) (FormulaWith assertion)
  | Equivalent (FormulaWith assertion) (FormulaWith assertion)
  | Release (FormulaWith assertion) (FormulaWith assertion)
  deriving (Show)

type Formula = FormulaWith QueryAssertion

depth :: FormulaWith a -> Int
depth = \case
  True -> 0
  False -> 0
  Not p -> succ (depth p)
  Eventually p -> succ (depth p)
  Always p -> succ (depth p)
  And p q -> succ (max (depth p) (depth q))
  Or p q -> succ (max (depth p) (depth q))
  Until p q -> succ (max (depth p) (depth q))
  Release p q -> succ (max (depth p) (depth q))
  Implies p q -> succ (max (depth p) (depth q))
  Equivalent p q -> succ (max (depth p) (depth q))
  Assert _ -> 0

simplify :: FormulaWith a -> Minimal.FormulaWith a
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
  Assert assertion -> Minimal.Assert assertion

toNNF :: FormulaWith a -> NNF.FormulaWith (NNF.Negation a)
toNNF = \case
  -- Negation propagation (https://en.wikipedia.org/wiki/Linear_temporal_logic#Negation_normal_form)
  Not (Not p) -> toNNF p
  Not True -> NNF.False
  Not False -> NNF.True
  Not (p `Or` q) -> toNNF (Not p) `NNF.And` toNNF (Not q)
  Not (p `And` q) -> toNNF (Not p) `NNF.Or` toNNF (Not q)
  Not (p `Implies` q) -> toNNF p `NNF.And` toNNF (Not q)
  Not (p `Equivalent` q) -> toNNF (p `Equivalent` Not q)
  Not (Until p q) -> toNNF (Not p) `NNF.Release` toNNF (Not q)
  Not (Release p q) -> toNNF (Not p) `NNF.Until` toNNF (Not q)
  Not (Eventually p) -> toNNF (Always (Not p))
  Not (Always p) -> toNNF (Eventually (Not p))
  Not (Assert assertion) -> NNF.Assert (NNF.Neg assertion)
  -- Derived operators (only present in syntax) are simplified
  Eventually p -> NNF.Until NNF.True (toNNF p)
  Always p -> NNF.False `NNF.Release` toNNF p
  Implies p q -> toNNF (Not p `Or` q)
  Equivalent p q -> (toNNF (Not p) `NNF.Or` toNNF q) `NNF.Or` (toNNF (Not q) `NNF.Or` toNNF p)
  -- Language operators that are preserved
  True -> NNF.True
  False -> NNF.False
  Or p q -> NNF.Or (toNNF p) (toNNF q)
  And p q -> NNF.And (toNNF p) (toNNF q)
  Until p q -> NNF.Until (toNNF p) (toNNF q)
  Release p q -> NNF.Release (toNNF p) (toNNF q)
  Assert assertion -> NNF.Assert (NNF.Pos assertion)

infix 4 \/, /\, ∧, ∨

(/\), (\/), (∧), (∨) :: Formula -> Formula -> Formula
(/\) = And
(\/) = Or
(∧) = And
(∨) = Or

infix 5 ===, ≡

(===), (≡) :: (Show a, Eq a) => Eff '[Query] a -> a -> Formula
query' === expected = Assert (QueryAssertion query' (Equals expected))
(≡) = (===)

(⊢) :: Show a => Eff '[Query] a -> (a -> Bool) -> Formula
query' ⊢ f = Assert (QueryAssertion query' (Satisfies f))

infix 6 ¬

not, (¬) :: Formula -> Formula
not = Not
(¬) = Not
