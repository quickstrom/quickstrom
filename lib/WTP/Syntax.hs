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
    require,
  )
where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Data.Bool                 (Bool)
import           Data.Text                 (Text)
import           Prelude                 hiding ( Bool(..)
                                                , not
                                                )
import qualified WTP.Formula                   as Simple
import           WTP.Query                 as Query

type QueryEff = Eff '[Query, Error Text]

data Formula where
  -- Simplified language operators
  True :: Formula
  Not :: Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Until :: Formula -> Formula -> Formula
  Assert :: Show a => QueryEff a -> Simple.Assertion a -> Formula
  -- Full language operators
  False :: Formula
  Eventually :: Formula -> Formula
  Always :: Formula -> Formula
  And :: Formula -> Formula -> Formula
  Implies :: Formula -> Formula -> Formula
  Equivalent :: Formula -> Formula -> Formula
  Release :: Formula -> Formula -> Formula

simplify :: Formula -> Simple.Formula
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

(/\), (\/), (∧), (∨) :: Formula -> Formula -> Formula
(/\) = And
(\/) = Or
(∧) = And
(∨) = Or

infix 5 ===, ≡

(===), (≡) :: (Show a, Eq a) => QueryEff a -> a -> Formula
query === expected = Assert query (Simple.Equals expected)
query ≡ expected = Assert query (Simple.Equals expected)

(⊢) :: (Show a) => QueryEff a -> (a -> Bool) -> Formula
query ⊢ f = Assert query (Simple.Satisfies f)

infix 6 ¬

not, (¬) :: Formula -> Formula
not = Not
(¬) = Not

require :: Maybe a -> QueryEff a
require = maybe (throwError ("Required value is Nothing" :: Text)) pure

