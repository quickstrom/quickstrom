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
import Data.HashSet (HashSet)
import Data.String (IsString (..))
import qualified Data.Text as Text
import GHC.Exts (IsList (..))
import WTP.Query
import WTP.Value
import Prelude hiding (not)

type Set = HashSet

data Comparison
  = LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  deriving (Show, Eq, Ord, Bounded)

data QueryCardinality = QueryOne | QueryAll
  deriving (Eq, Show, Ord, Enum, Bounded)

data Formula
  = Literal Value
  | Set [Formula]
  | Seq [Formula]
  | Next Formula
  | Always Formula
  | BindQuery QueryCardinality Query
  | Apply Formula [Formula]
  | Equals Formula Formula
  | Compare Comparison Formula Formula
  deriving (Eq, Show)

instance IsString Formula where
  fromString = Literal . VString . Text.pack

instance IsList Formula where
  type Item Formula = Formula
  fromList = Seq
  toList = \case
    Seq xs -> xs
    _ -> []

instance Lattice Formula where
  p /\ q = Apply (Literal (VFunction (BuiltInFunction FAnd))) [p, q]
  p \/q  = Apply (Literal (VFunction (BuiltInFunction FOr))) [p, q]

instance BoundedJoinSemiLattice Formula where
  bottom = Literal (VBool False)

instance BoundedMeetSemiLattice Formula where
  top = Literal (VBool True)

instance Heyting Formula where
  p ==> q = neg p \/ q
  neg p = (Apply (Literal (VFunction (BuiltInFunction FNot))) [p])

simplify :: Formula -> Formula
simplify f@(Apply (Literal (VFunction (BuiltInFunction FAnd))) [p, q]) =
  case (simplify p, simplify q) of
    (_, Literal (VBool False)) -> Literal (VBool False)
    (Literal (VBool False), _) -> Literal (VBool False)
    (p', Literal (VBool True)) -> p'
    (Literal (VBool True), p') -> p'
    _ -> f
simplify f@(Apply (Literal (VFunction (BuiltInFunction FOr))) [p, q]) =
  case (simplify p, simplify q) of
    (_, Literal (VBool True)) -> Literal (VBool True)
    (Literal (VBool True), _) -> Literal (VBool True)
    (p', Literal (VBool False)) -> p'
    (Literal (VBool False), p') -> p'
    _ -> f
simplify f@(Apply (Literal (VFunction (BuiltInFunction FNot))) [p]) =
  case simplify p of
    Literal (VBool False) -> Literal (VBool True)
    Literal (VBool True) -> Literal (VBool False)
    _ -> f
simplify p = p

withQueries :: Monad m => (Query -> m b) -> Formula -> m [b]
withQueries f = \case
  Literal {} -> pure []
  Set ps -> concat <$> traverse (withQueries f) ps
  Seq ps -> concat <$> traverse (withQueries f) ps
  -- Not p -> withQueries f p
  -- And p q -> (<>) <$> withQueries f p <*> withQueries f q
  -- Or p q -> (<>) <$> withQueries f p <*> withQueries f q
  Next p -> withQueries f p
  Always p -> withQueries f p
  Apply f' a -> concat <$> traverse (withQueries f) (f' : a)
  Equals p q -> (<>) <$> withQueries f p <*> withQueries f q
  Compare _ p q -> (<>) <$> withQueries f p <*> withQueries f q
  BindQuery _ query -> pure <$> f query
