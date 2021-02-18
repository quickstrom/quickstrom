{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Quickstrom.StreamingVerifier where

import Algebra.Heyting
import Algebra.Lattice
import GHC.Show
import Numeric.Natural
import Quickstrom.Prelude hiding (State, and, negate, not, show)

-- * Language

data Certainty a = Definitely a | Probably a
  deriving (Show, Functor)

instance Lattice a => Lattice (Certainty a) where
  Definitely a /\ Definitely b = Definitely (a /\ b)
  Probably a /\ Definitely b = Probably (a /\ b)
  Definitely a /\ Probably b = Probably (a /\ b)
  Probably a /\ Probably b = Probably (a /\ b)

  Definitely a \/ Definitely b = Definitely (a \/ b)
  Probably a \/ Definitely b = Definitely (a \/ b)
  Definitely a \/ Probably b = Definitely (a \/ b)
  Probably a \/ Probably b = Probably (a \/ b)

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Certainty a) where
  top = Definitely top

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Certainty a) where
  bottom = Definitely bottom

instance Heyting a => Heyting (Certainty a) where
  f1 ==> f2 = neg f1 /\ f2
  neg = map neg

type Result = Certainty Bool

type State = Char

type Trace = [State]

data Formula
  = Trivial
  | Absurd
  | Atomic (State -> Bool)
  | And Formula Formula
  | Or Formula Formula
  | -- | strong next
    Next Formula
  | -- | weak next
    WNext Formula
  | -- | demanding next
    DNext Formula

instance Show Formula where
  show = \case
    Trivial -> "Trivial"
    Absurd -> "Absurd"
    Atomic _ -> "Atomic"
    And p q -> "(And " <> show p <> " " <> show q <> ")"
    Or p q -> "(Or " <> show p <> " " <> show q <> ")"
    Next p -> "(Next " <> show p <> ")"
    WNext _ -> "(WNext ...)"
    DNext p -> "(DNext " <> show p <> ")"

-- * Syntax

is :: Char -> Formula
is c = Atomic (== c)

next :: Formula -> Formula
next = Next

wnext :: Formula -> Formula
wnext = WNext

always :: Natural -> Formula -> Formula
always 0 f = f /\ WNext (always 0 f)
always n f = f /\ DNext (always (pred n) f)

eventually :: Natural -> Formula -> Formula
eventually 0 f = f \/ Next (eventually 0 f)
eventually n f = f \/ DNext (eventually (pred n) f)

instance Lattice Formula where
  Absurd /\ _ = Absurd
  _ /\ Absurd = Absurd
  Trivial /\ x = x
  x /\ Trivial = x
  x /\ y = And x y

  Absurd \/ x = x
  x \/ Absurd = x
  Trivial \/ _ = Trivial
  _ \/ Trivial = Trivial
  x \/ y = Or x y

instance BoundedMeetSemiLattice Formula where
  top = Trivial

instance BoundedJoinSemiLattice Formula where
  bottom = Absurd

-- * Evaluation

step :: Formula -> State -> Formula
step Trivial _ = Trivial
step Absurd _ = Absurd
step (Atomic a) s = if a s then Trivial else Absurd
step (And f1 f2) s = step f1 s /\ step f2 s
step (Or f1 f2) s = step f1 s \/ step f2 s
step (Next f) _ = f
step (WNext f) _ = f
step (DNext f) _ = f

requiresMoreStates :: Formula -> Bool
requiresMoreStates = \case
  Trivial -> False
  Absurd -> False
  Atomic {} -> False
  (And f1 f2) -> requiresMoreStates f1 \/ requiresMoreStates f2
  (Or f1 f2) -> requiresMoreStates f1 \/ requiresMoreStates f2
  Next {} -> False
  WNext {} -> False
  DNext {} -> True

stepRequired :: Formula -> Trace -> Formula
stepRequired f (x:xs) | requiresMoreStates f = stepRequired (step f x) xs
stepRequired f _ = f

-- TODO
stepResidual :: Formula -> Trace -> Formula
stepResidual f (x:xs) = stepRequired (step f x) xs
stepResidual f _ = f

compute :: Formula -> Maybe Result
compute = \case
  Trivial -> Just (Definitely True)
  Absurd -> Just (Definitely False)
  Atomic {} -> Nothing
  Next {} -> Just (Probably False)
  WNext {} -> Just (Probably True)
  DNext {} -> Nothing
  And p q -> liftA2 (/\) (compute p) (compute q)
  Or p q -> liftA2 (\/) (compute p) (compute q)

verify :: Formula -> Trace -> Maybe Result
verify f t = 
  let f' = stepRequired f t
  in compute f'
