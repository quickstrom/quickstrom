{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module WTP.Formula where

import Algebra.Heyting (Heyting (..))
import Algebra.Lattice (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))
import qualified Data.Aeson as JSON
import Data.HashSet (HashSet)
import Data.String (IsString (..))
import qualified Data.Text as Text
import WTP.Query
import Prelude hiding (not)
import GHC.Exts (IsList (..))

type Set = HashSet

data Comparison
  = LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  deriving (Show, Eq, Ord, Bounded)

type Value = JSON.Value

data QueryCardinality = QueryOne | QueryAll
  deriving (Eq, Show, Ord, Enum, Bounded)

data Formula
  = Literal Value
  | Set [Formula]
  | Seq [Formula]
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Next Formula
  | Always Formula
  | BindQuery QueryCardinality Query
  | Equals Formula Formula
  | Compare Comparison Formula Formula
  deriving (Eq, Show)

instance IsString Formula where
  fromString = Literal . JSON.String . Text.pack

instance IsList Formula where
  type Item Formula = Formula
  fromList = Seq
  toList = \case
    Seq xs -> xs
    _ -> []

instance Lattice Formula where

  (/\) = And

  (\/) = Or

instance BoundedJoinSemiLattice Formula where
  bottom = Literal (JSON.Bool False)

instance BoundedMeetSemiLattice Formula where
  top = Literal (JSON.Bool True)

instance Heyting Formula where
  p ==> q = Not p `Or` q
  neg = Not

simplify :: Formula -> Formula
simplify = \case
  And p q ->
    case (simplify p, simplify q) of
      (_, Literal (JSON.Bool False)) -> Literal (JSON.Bool False)
      (Literal (JSON.Bool False), _) -> Literal (JSON.Bool False)
      (p', Literal (JSON.Bool True)) -> p'
      (Literal (JSON.Bool True), p') -> p'
      (p', q') -> And p' q'
  Or p q ->
    case (simplify p, simplify q) of
      (_, Literal (JSON.Bool True)) -> Literal (JSON.Bool True)
      (Literal (JSON.Bool True), _) -> Literal (JSON.Bool True)
      (p', Literal (JSON.Bool False)) -> p'
      (Literal (JSON.Bool False), p') -> p'
      (p', q') -> Or p' q'
  Not p ->
    case simplify p of
      Literal (JSON.Bool False) -> Literal (JSON.Bool True)
      Literal (JSON.Bool True) -> Literal (JSON.Bool False)
      p' -> Not p'
  p -> p

withQueries :: Monad m => (Query -> m b) -> Formula -> m [b]
withQueries f = \case
  Literal {} -> pure []
  Set ps -> concat <$> traverse (withQueries f) ps
  Seq ps -> concat <$> traverse (withQueries f) ps
  Not p -> withQueries f p
  And p q -> (<>) <$> withQueries f p <*> withQueries f q
  Or p q -> (<>) <$> withQueries f p <*> withQueries f q
  Next p -> withQueries f p
  Always p -> withQueries f p
  Equals p q -> (<>) <$> withQueries f p <*> withQueries f q
  Compare _ p q -> (<>) <$> withQueries f p <*> withQueries f q
  BindQuery _ query -> pure <$> f query
