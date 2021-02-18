{-# LANGUAGE DeriveFunctor #-}

module Quickstrom.StreamingVerifier where

import qualified Data.Bool
import Quickstrom.Prelude hiding (State, and, negate, not)

-- * Language

type State = Char

data Value b
  = Done b
  | Continue (State -> Value b)
  deriving (Functor)

(/\) :: Value Bool -> Value Bool -> Value Bool
Done b1 /\ Done b2 = Done (b1 && b2)
Continue f1 /\ Continue f2 = Continue (\s -> f1 s /\ f2 s)
Continue f1 /\ v2 = Continue (\s -> f1 s /\ v2)
v1 /\ Continue f2 = Continue (\s -> v1 /\ f2 s)

negate :: Value Bool -> Value Bool
negate = map Data.Bool.not

data Formula
  = Atomic (Char -> Bool)
  | And Formula Formula
  | Next Formula
  | Not Formula

-- * Syntax

is :: Char -> Formula
is c = Atomic (== c)

next :: Formula -> Formula
next = Next

not :: Formula -> Formula
not = Not

and :: Formula -> Formula -> Formula
and = And

or :: Formula -> Formula -> Formula
or f1 f2 = not (and (not f1) (not f2))

-- * Evaluation

eval :: Formula -> State -> Value Bool
eval (Atomic a) s = Done (a s)
eval (And f1 f2) s = eval f1 s /\ eval f2 s
eval (Next f) _ = Continue (eval f)
eval (Not f) s = negate (eval f s)

evalList :: Formula -> [State] -> Maybe Bool
evalList _ [] = Nothing
evalList f (x : xs) = go (eval f x) xs
  where
    go (Done b) _ = Just b
    go _ [] = Nothing
    go (Continue c) (x' : xs') = go (c x') xs'
