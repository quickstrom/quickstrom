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

module WTP.Syntax
  ( Simple.Assertion (..)
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
import qualified WTP.Formula                    as Simple
import           WTP.Query                 as Query

data Formula (eff :: [* -> *]) where
  -- Simplified language operators
  True :: Formula eff
  Not :: Formula eff -> Formula eff
  Or :: Formula eff -> Formula eff -> Formula eff
  Until :: Formula eff -> Formula eff -> Formula eff
  Assert :: (Show a, Simple.IsQuery eff) => Eff eff a -> Simple.Assertion a -> Formula eff
  -- Full language operators
  False :: Formula eff
  Eventually :: Formula eff -> Formula eff
  Always :: Formula eff -> Formula eff
  And :: Formula eff -> Formula eff -> Formula eff
  Implies :: Formula eff -> Formula eff -> Formula eff
  Equivalent :: Formula eff -> Formula eff -> Formula eff
  Release :: Formula eff -> Formula eff -> Formula eff

simplify :: Formula eff -> Simple.Formula eff
simplify = \case
  -- Derived operators (only present in `Full` language) are simplified
  False -> Simple.Not Simple.True
  Eventually p -> Simple.Until Simple.True (simplify p)
  Always p -> Simple.Not (simplify (Eventually (Not p)))
  And p q -> Simple.Not (Simple.Not (simplify p) `Simple.Or` Simple.Not (simplify q))
  Implies p q -> Simple.Not (simplify p) `Simple.Or` simplify q
  Equivalent p q -> simplify (p `Implies` q) `Simple.Or` simplify (q `Implies` p)
  Release p q -> Simple.Not (Simple.Not (simplify p) `Simple.Until` Simple.Not (simplify q))
  -- Simplified language operators are preserved
  True -> Simple.True
  Not p -> Simple.Not (simplify p)
  Or p q -> Simple.Or (simplify p) (simplify q)
  Until p q -> Simple.Until (simplify p) (simplify q)
  Assert query assertion -> Simple.Assert query assertion

infix 4 \/, /\, ∧, ∨

(/\), (\/), (∧), (∨) :: Formula eff -> Formula eff -> Formula eff
(/\) = And
(\/) = Or
(∧) = And
(∨) = Or

infix 5 ===, ≡

(===), (≡) :: (Show a, Eq a, Simple.IsQuery eff) => Eff eff a -> a -> Formula eff
query === expected = Assert query (Simple.Equals expected)
query ≡ expected = Assert query (Simple.Equals expected)

(⊢) :: (Show a, Simple.IsQuery eff) => Eff eff a -> (a -> Bool) -> Formula eff
query ⊢ f = Assert query (Simple.Satisfies f)

infix 6 ¬

not, (¬) :: Formula eff -> Formula eff
not = Not
(¬) = Not

-- require :: Simple.IsQuery eff => Maybe a -> Eff eff a
-- require = maybe (throwError ("Required value is Nothing" :: Text)) pure

