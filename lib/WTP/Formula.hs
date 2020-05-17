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
import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import WTP.Query
import Prelude hiding (False, True, not)

data Literal t where
  LTrue :: Literal Bool
  LFalse :: Literal Bool
  LNum :: (Eq n, Show n, Num n) => n -> Literal n
  LString :: Text -> Literal Text
  LJson :: JSON.Value -> Literal JSON.Value

deriving instance Eq (Literal t)

deriving instance Show (Literal t)

type IsValue a = (Eq a, Show a, Typeable a)

type Set = HashSet

data Comparison
  = LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual

data Formula t where
  Literal :: Literal a -> Formula a
  Set :: (IsValue a, Hashable a) => [Formula a] -> Formula (Set a)
  Seq :: IsValue a => [Formula a] -> Formula [a]
  Not :: Formula Bool -> Formula Bool
  And :: Formula Bool -> Formula Bool -> Formula Bool
  Or :: Formula Bool -> Formula Bool -> Formula Bool
  Next :: Formula a -> Formula a
  Always :: Formula Bool -> Formula Bool
  BindQuery :: (IsValue a, Hashable a) => Query a -> Formula [a]
  Equals :: (a ~ b, IsValue a, IsValue b) => Formula a -> Formula b -> Formula Bool
  Compare :: Ord a => Comparison -> Formula a -> Formula a -> Formula Bool
  -- ForAll :: Formula (Set a) -> (FValue a -> Formula Bool) -> Formula Bool

  MapFormula :: (a -> b) -> Formula a -> Formula b

instance Show (Formula a) where
  show = \case
    Literal l -> show l
    Set ps -> show ps
    Seq ps -> show ps
    Not p -> "(Not " <> show p <> ")"
    And p q -> "(And " <> show p <> " " <> show q <> ")"
    Or p q -> "(Or " <> show p <> " " <> show q <> ")"
    Next p -> "(Next " <> show p <> ")"
    Always p -> "(Always " <> show p <> ")"
    BindQuery q -> "(BindQuery " <> show q <> ")"
    Equals p q -> "(Equals " <> show p <> " " <> show q <> ")"
    Compare comp p q ->
      let op = case comp of
            LessThan -> "<"
            LessThanEqual -> "<="
            GreaterThan -> ">"
            GreaterThanEqual -> ">="
       in "(" <> show p <> " " <> op <> " " <> show q <> ")"
    MapFormula _ p -> "(MapFormula _ " <> show p <> ")"

instance Functor Formula where
  fmap = MapFormula

instance IsString (Formula Text) where
  fromString = Literal . LString . Text.pack

type Proposition = Formula Bool

instance Lattice Proposition where

  (/\) = And

  (\/) = Or

instance BoundedJoinSemiLattice Proposition where
  bottom = Literal LFalse

instance BoundedMeetSemiLattice Proposition where
  top = Literal LTrue

instance Heyting Proposition where
  p ==> q = Not p `Or` q
  neg = Not

simplify :: Formula a -> Formula a
simplify = \case
  And p q ->
    case (simplify p, simplify q) of
      (_, Literal LFalse) -> Literal LFalse
      (Literal LFalse, _) -> Literal LFalse
      (p', Literal LTrue) -> p'
      (Literal LTrue, p') -> p'
      (p', q') -> And p' q'
  Or p q ->
    case (simplify p, simplify q) of
      (_, Literal LTrue) -> Literal LTrue
      (Literal LTrue, _) -> Literal LTrue
      (p', Literal LFalse) -> p'
      (Literal LFalse, p') -> p'
      (p', q') -> Or p' q'
  Not p ->
    case simplify p of
      Literal LFalse -> Literal LTrue
      Literal LTrue -> Literal LFalse
      p' -> Not p'
  p -> p

withQueries :: (Monad m, IsValue b) => (forall q. (IsValue q, Hashable q) => Query q -> m b) -> Formula a -> m [b]
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
  BindQuery query -> pure <$> f query
  MapFormula _ p -> withQueries f p
