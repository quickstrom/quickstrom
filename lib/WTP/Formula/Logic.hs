{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module WTP.Formula.Logic where

import Data.Set (Set)
import Data.Text (Text)
import WTP.Element
import WTP.Result
import qualified WTP.Type as WTP
import Prelude hiding (False, True, not)
import Algebra.Lattice (Lattice(..), BoundedMeetSemiLattice(..), BoundedJoinSemiLattice(..))
import Algebra.Heyting (Heyting(..))

data Negation a = Neg a | Pos a
  deriving (Eq, Show, Functor, Foldable, Traversable)

withAtomic :: (a -> b) -> Negation a -> b
withAtomic f (Neg a) = f a
withAtomic f (Pos a) = f a

data FValue (t :: WTP.Type) where
  VTrue :: FValue 'WTP.Bool
  VFalse :: FValue 'WTP.Bool
  VString :: Text -> FValue 'WTP.String
  VElement :: Element -> FValue 'WTP.Element
  VSet :: Set (FValue a) -> FValue ('WTP.Set a)
  VSeq :: [FValue a] -> FValue ('WTP.Seq a)

instance Lattice (FValue 'WTP.Bool) where
  VTrue /\ p = p
  p /\ VTrue = p
  VFalse /\ _ = VFalse
  _ /\ VFalse = VFalse

  VFalse \/ p = p
  p \/ VFalse = p
  _ \/ _ = p

instance BoundedJoinSemiLattice (FValue 'WTP.Bool) where
  bottom = VFalse

instance BoundedMeetSemiLattice (FValue 'WTP.Bool) where
  top = VTrue

instance Heyting (FValue 'WTP.Bool) where
  VFalse ==> _ = VTrue
  VTrue ==> q = q

deriving instance Show (FValue t)

deriving instance Eq (FValue t)

data Formula t where
  Literal :: FValue a -> Formula a
  Not :: Formula 'WTP.Bool -> Formula 'WTP.Bool
  And :: Formula 'WTP.Bool -> Formula 'WTP.Bool -> Formula 'WTP.Bool
  Or :: Formula 'WTP.Bool -> Formula 'WTP.Bool -> Formula 'WTP.Bool
  Always :: Formula 'WTP.Bool -> Formula 'WTP.Bool
  -- ForAll :: Formula (Set a) -> (FValue a -> Formula Bool) -> Formula Bool
  Get :: ElementState a -> Formula 'WTP.Element -> Formula a
  Equals :: a ~ b => Formula a -> Formula b -> Formula 'WTP.Bool
  Query :: Selector -> Formula ('WTP.Seq 'WTP.Element)

deriving instance Show (Formula t)

type Proposition = Formula 'WTP.Bool

instance Lattice Proposition where
  (/\) = And
  (\/) = Or

instance BoundedJoinSemiLattice Proposition where
  bottom = Literal VFalse

instance BoundedMeetSemiLattice Proposition where
  top = Literal VTrue

instance Heyting Proposition where
  p ==> q = Not p `Or` q

simplify :: Formula a -> Formula a
simplify = \case
  And p q ->
    case (simplify p, simplify q) of
      (_, Literal VFalse) -> Literal VFalse
      (Literal VFalse, _) -> Literal VFalse
      (p', Literal VTrue) -> p'
      (Literal VTrue, p') -> p'
      (p', q') -> And p' q'
  Or p q ->
    case (simplify p, simplify q) of
      (_, Literal VTrue) -> Literal VTrue
      (Literal VTrue, _) -> Literal VTrue
      (p', Literal VFalse) -> p'
      (Literal VFalse, p') -> p'
      (p', q') -> Or p' q'
  Not (Literal VFalse) -> Literal VTrue
  Not (Literal VTrue) -> Literal VFalse
  Not (Not p) -> simplify p
  p -> p

eval :: Formula a -> [step] -> FValue a
eval f steps = case f of
  Literal v -> v
  Not p -> neg (eval p steps)
  Literal v -> v
  Literal v -> v
  Literal v -> v

verify :: Formula 'WTP.Bool -> [step] -> Result
verify _ _ = Accepted
{-
go
  where
    go _ [] = Rejected
    go True _ = Accepted
    go False _ = Rejected
    go (p `Or` q) trace = go p trace \/ go q trace
    go (p `And` q) trace = go p trace /\ go q trace
    go (Always p) trace = go p trace /\ go p (tail trace)
    go (Equals f1 f2) (current : _) =
      case a of
        Pos a' -> assert a' current
        Neg a' -> neg (assert a' current)

-}
