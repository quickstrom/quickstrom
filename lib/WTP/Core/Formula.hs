{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

module WTP.Core.Formula where

import Algebra.Heyting (Heyting (..))
import Algebra.Lattice (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))
import Data.String (IsString (..))
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import WTP.Core.Query
import WTP.Core.Value
import Prelude hiding (not)

type IsValue a = (Eq a, Show a, Typeable a)

data Comparison
  = LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  deriving (Eq, Show, Generic)

data Formula
  = Literal Value
  | Set [Formula]
  | Seq [Formula]
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Next Formula
  | Always Formula
  | QueryOne Query
  | QueryAll Query
  | Equals Formula Formula
  | Compare Comparison Formula Formula
  deriving (Eq, Show, Generic)

instance IsString Formula where
  fromString = Literal . VString . Text.pack

instance Lattice Formula where

  (/\) = And

  (\/) = Or

instance BoundedJoinSemiLattice Formula where
  bottom = Literal (VBoolean False)

instance BoundedMeetSemiLattice Formula where
  top = Literal (VBoolean True)

instance Heyting Formula where

  p ==> q = Not p `Or` q

  neg = Not

simplify :: Formula -> Formula
simplify = \case
  And p q ->
    case (simplify p, simplify q) of
      (_, Literal (VBoolean False)) -> Literal (VBoolean False)
      (Literal (VBoolean False), _) -> Literal (VBoolean False)
      (p', Literal (VBoolean True)) -> p'
      (Literal (VBoolean True), p') -> p'
      (p', q') -> And p' q'
  Or p q ->
    case (simplify p, simplify q) of
      (_, Literal (VBoolean True)) -> Literal (VBoolean True)
      (Literal (VBoolean True), _) -> Literal (VBoolean True)
      (p', Literal (VBoolean False)) -> p'
      (Literal (VBoolean False), p') -> p'
      (p', q') -> Or p' q'
  Not p ->
    case simplify p of
      Literal (VBoolean False) -> Literal (VBoolean True)
      Literal (VBoolean True) -> Literal (VBoolean False)
      p' -> Not p'
  p -> p

withQueries :: (Monad m, IsValue b) => (Query -> m b) -> Formula -> m [b]
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
  QueryAll query -> pure <$> f query
  QueryOne query -> pure <$> f query
