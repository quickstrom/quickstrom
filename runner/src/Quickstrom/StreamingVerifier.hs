{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Quickstrom.StreamingVerifier where

import Numeric.Natural
import Algebra.Heyting
import Algebra.Lattice
import Quickstrom.Prelude hiding (State, and, negate, not, show)
import GHC.Show

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
  -- | strong next
  | Next Formula
  -- | weak next
  | WNext Formula
  -- | demanding next
  | DNext Formula

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
  (/\) = And
  (\/) = Or

instance BoundedMeetSemiLattice Formula where
  top = Trivial

instance BoundedJoinSemiLattice Formula where
  bottom = Absurd

-- * Evaluation

step :: Formula -> State -> Formula
step Trivial _ = Trivial
step Absurd _ = Trivial
step (Atomic a) s = if a s then Trivial else Absurd
step (And f1 f2) s =
  case step f1 s of
    Trivial -> step f2 s
    Absurd -> Absurd
    r1 ->
      case step f2 s of
        Trivial -> Trivial
        Absurd -> Absurd
        r2 -> r1 /\ r2
step (Or f1 f2) s = step f1 s \/ step f2 s -- TODO: ???
step (Next f) _ = f
step (WNext f) _ = f
step (DNext f) _ = f

verify :: Formula -> Trace -> Maybe Result
verify = go 
  where
    go = \case
      Trivial -> const (Just (Definitely True))
      Absurd -> const (Just (Definitely False))
      a@Atomic{} -> \case
        [] -> Nothing
        x' : xs' -> go (step a x') xs'
      Next p -> \case
        [] -> Just (Probably False)
        x' : xs' -> go (step p x') xs'
      WNext p -> \case
        [] -> Just (Probably True)
        x' : xs' -> go (step p x') xs'
      DNext p -> \case
        [] ->  Nothing
        x' : xs' -> go (step p x') xs'
      And p q -> \xs' -> liftA2 (/\) (go p xs') (go q xs')
      Or p q -> \xs' -> liftA2 (\/) (go p xs') (go q xs')
