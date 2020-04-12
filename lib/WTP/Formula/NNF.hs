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
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Formula.NNF where

import Algebra.Heyting (Heyting (neg))
import Algebra.Lattice
import Control.Monad.Freer
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import WTP.Assertion
import WTP.Query
import WTP.Result
import Prelude hiding (False, True, not)

data Negation a = Neg a | Pos a
  deriving (Show, Functor, Foldable, Traversable)

withAtomic :: (a -> b) -> Negation a -> b
withAtomic f (Neg a) = f a
withAtomic f (Pos a) = f a

data FormulaWith assertion
  = True
  | False
  | And (FormulaWith assertion) (FormulaWith assertion)
  | Or (FormulaWith assertion) (FormulaWith assertion)
  | Until (FormulaWith assertion) (FormulaWith assertion)
  | Release (FormulaWith assertion) (FormulaWith assertion)
  | Assert assertion
  deriving (Show)

depth :: FormulaWith a -> Int
depth = \case
  True -> 0
  False -> 0
  And p q -> succ (max (depth p) (depth q))
  Or p q -> succ (max (depth p) (depth q))
  Until p q -> succ (max (depth p) (depth q))
  Release p q -> succ (max (depth p) (depth q))
  Assert _ -> 0

type Formula = FormulaWith (Negation QueryAssertion)

withQueries :: Monad m => (forall a. Eff '[Query] a -> m b) -> Formula -> m [b]
withQueries f = \case
  True -> pure []
  False -> pure []
  And p q -> (<>) <$> withQueries f p <*> withQueries f q
  Or p q -> (<>) <$> withQueries f p <*> withQueries f q
  Until p q -> (<>) <$> withQueries f p <*> withQueries f q
  Release p q -> (<>) <$> withQueries f p <*> withQueries f q
  Assert a -> (: []) <$> withAtomic (\(QueryAssertion q _) -> f q) a

data VerifiedStep a step
  = VerifiedStep
      { step :: Maybe step,
        stepFormula :: FormulaWith (Negation a),
        stepResult :: Result
      }
  deriving (Show)

verifyWith :: (a -> step -> Result) -> FormulaWith (Negation a) -> [step] -> Tree (VerifiedStep a step)
verifyWith assert = go
  where
    go spec steps =
      case steps of
        [] -> Tree.Node VerifiedStep {step = Nothing, stepFormula = spec, stepResult = Accepted} []
        current : rest ->
          let (result, substeps) = case spec of
                True -> (Accepted, [])
                False -> (Rejected, [])
                p `Or` q ->
                  let r1 = go p steps
                      r2 = go q steps
                   in (stepResult (Tree.rootLabel r1) \/ stepResult (Tree.rootLabel r2), [r1, r2])
                p `And` q ->
                  let r1 = go p steps
                      r2 = go q steps
                   in (stepResult (Tree.rootLabel r1) /\ stepResult (Tree.rootLabel r2), [r1, r2])
                p `Until` q ->
                  let s1 = go p [current]
                      s2 = go q rest
                      r = case (stepResult (Tree.rootLabel s1), stepResult (Tree.rootLabel s2)) of
                        (Accepted, Rejected {}) -> stepResult (Tree.rootLabel (go (p `Until` q) rest))
                        (Accepted, r2) -> r2
                        (_, Accepted) -> Accepted
                        (r1, _) -> r1
                   in (r, [s1, s2])
                p `Release` q ->
                  let s1 = go p [current]
                      s2 = go q steps
                      r = case (stepResult (Tree.rootLabel s1), stepResult (Tree.rootLabel s2)) of
                        (Accepted, Accepted) -> Accepted
                        (Accepted, _) -> stepResult (Tree.rootLabel (go spec rest))
                        (_, Accepted) -> Accepted
                        (r1, _) -> r1
                   in (r, [s1, s2])
                Assert a ->
                  let r = case a of
                        Pos a' -> assert a' current
                        Neg a' -> neg (assert a' current)
                   in (r, [])
           in Tree.Node VerifiedStep {step = Just current, stepResult = result, stepFormula = spec} substeps

verify :: Eq a => FormulaWith (Negation a) -> [a] -> Tree (VerifiedStep a a)
verify = verifyWith $ \a b -> fromBool (a == b)