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
  ( Core.Assertion (..)
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
import           Prelude                   hiding (Bool (..), not)
import qualified WTP.Core                  as Core
import           WTP.Query                 as Query

type QueryEff = Eff '[Query, Error Text]

data Formula where
  -- Simplified language operators
  True :: Formula
  Not :: Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Until :: Formula -> Formula -> Formula
  Assert :: Show a => QueryEff a -> Core.Assertion a -> Formula
  -- Full language operators
  False :: Formula
  Eventually :: Formula -> Formula
  Always :: Formula -> Formula
  And :: Formula -> Formula -> Formula
  Implies :: Formula -> Formula -> Formula
  Equivalent :: Formula -> Formula -> Formula
  Release :: Formula -> Formula -> Formula

simplify :: Formula -> Core.Formula
simplify = \case
  -- Derived operators (only present in `Full` language) are simplified
  False -> Core.Not Core.True
  Eventually p -> Core.Until Core.True (simplify p)
  Always p -> Core.Not (simplify (Eventually (Not p)))
  And p q -> Core.Not (Core.Not (simplify p) `Core.Or` Core.Not (simplify q))
  Implies p q -> Core.Not (simplify p) `Core.Or` simplify q
  Equivalent p q -> simplify (p `Implies` q) `Core.Or` simplify (q `Implies` p)
  Release p q -> Core.Not (Core.Not (simplify p) `Core.Until` Core.Not (simplify q))
  -- Simplified language operators are preserved
  True -> Core.True
  Not p -> Core.Not (simplify p)
  Or p q -> Core.Or (simplify p) (simplify q)
  Until p q -> Core.Until (simplify p) (simplify q)
  Assert query assertion -> Core.Assert query assertion

infix 4 \/, /\, ∧, ∨

(/\), (\/), (∧), (∨) :: Formula -> Formula -> Formula
(/\) = And
(\/) = Or
(∧) = And
(∨) = Or

infix 5 ===, ≡

(===), (≡) :: (Show a, Eq a) => QueryEff a -> a -> Formula
query === expected = Assert query (Core.Equals expected)
query ≡ expected = Assert query (Core.Equals expected)

(⊢) :: (Show a) => QueryEff a -> (a -> Bool) -> Formula
query ⊢ f = Assert query (Core.Satisfies f)

infix 6 ¬

not, (¬) :: Formula -> Formula
not = Not
(¬) = Not

require :: Maybe a -> QueryEff a
require = maybe (throwError ("Required value is Nothing" :: Text)) pure

