{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Quickstrom.StreamingVerifier where

import Algebra.Heyting
import Algebra.Lattice
import Quickstrom.Prelude hiding (State, and, negate, not)

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

data Value b
  = Done b
  | Continue (Maybe State -> Value b)
  deriving (Functor)

instance Lattice a => Lattice (Value a) where
  Done b1 /\ Done b2 = Done (b1 /\ b2)
  Continue f1 /\ Continue f2 = Continue (\s -> f1 s /\ f2 s)
  Continue f1 /\ v2 = Continue (\s -> f1 s /\ v2)
  v1 /\ Continue f2 = Continue (\s -> v1 /\ f2 s)

  Done b1 \/ Done b2 = Done (b1 \/ b2)
  Continue f1 \/ Continue f2 = Continue (\s -> f1 s \/ f2 s)
  Continue f1 \/ v2 = Continue (\s -> f1 s \/ v2)
  v1 \/ Continue f2 = Continue (\s -> v1 \/ f2 s)

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Value a) where
  top = Done top

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Value a) where
  bottom = Done bottom

instance Heyting a => Heyting (Value a) where
  a ==> b = map neg a \/ b

data Formula
  = Atomic (State -> Bool)
  | And Formula Formula
  | Next Formula
  | WNext Formula
  | Not Formula

-- * Syntax

is :: Char -> Formula
is c = Atomic (== c)

next :: Formula -> Formula
next = Next

wnext :: Formula -> Formula
wnext = WNext

always :: Formula -> Formula
always f = f /\ wnext (always f)

eventually :: Formula -> Formula
eventually f = f \/ next (eventually f)

instance Lattice Formula where
  (/\) = And
  f1 \/ f2 = neg (neg f1 /\ neg f2)

instance BoundedMeetSemiLattice Formula where
  top = Atomic (const top)

instance BoundedJoinSemiLattice Formula where
  bottom = Atomic (const bottom)

instance Heyting Formula where
  f1 ==> f2 = neg f1 /\ f2
  neg = Not

-- * Evaluation

eval :: Formula -> State -> Value Result
eval (Atomic a) s = Done (fromBool (a s))
eval (And f1 f2) s = eval f1 s /\ eval f2 s
eval (Next f) _ =
  Continue
    ( \case
        Just s -> eval f s
        Nothing -> Done (Probably False)
    )
eval (WNext f) _ =
  Continue
    ( \case
        Just s -> eval f s
        Nothing -> Done (Probably True)
    )
eval (Not f) s = neg (eval f s)

evalTrace :: Formula -> Trace -> Result
evalTrace _ [] = Definitely False
evalTrace f (x : xs) = go (eval f x) xs
  where
    go (Done b) _ = b
    go (Continue c) (x' : xs') = go (c (Just x')) xs'
    go (Continue c) [] = go (c Nothing) []
