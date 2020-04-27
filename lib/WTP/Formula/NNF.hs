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
  | Next (FormulaWith assertion)
  | Assert assertion
  deriving (Show)

type Formula = FormulaWith (Negation QueryAssertion)

withQueries :: Monad m => (forall a. Eff '[Query] a -> m b) -> Formula -> m [b]
withQueries f = \case
  True -> pure []
  False -> pure []
  And p q -> (<>) <$> withQueries f p <*> withQueries f q
  Or p q -> (<>) <$> withQueries f p <*> withQueries f q
  Until p q -> (<>) <$> withQueries f p <*> withQueries f q
  Release p q -> (<>) <$> withQueries f p <*> withQueries f q
  Next p -> withQueries f p
  Assert a -> (: []) <$> withAtomic (\(QueryAssertion q _) -> f q) a

verifyWith :: (a -> step -> Result) -> FormulaWith (Negation a) -> [step] -> Result
verifyWith assert = go
  where
    go _ [] = Rejected
    go True _ = Accepted
    go False _ = Rejected
    go (p `Or` q) trace = go p trace \/ go q trace
    go (p `And` q) trace = go p trace /\ go q trace
    go (p `Until` q) trace = go q trace \/ (go p trace /\ go (p `Until` q) (tail trace))
    go (p `Release` q) trace
      | length trace == 1 = go q trace
      | otherwise =
        case go p trace of
          Rejected -> go q trace /\ go (p `Release` q) (tail trace)
          Accepted -> go q trace
    go (Next False) [_] = Accepted
    go (Next _) [_] = Rejected
    go (Next p) trace = go p (tail trace)
    go (Assert a) (current : _) =
      case a of
        Pos a' -> assert a' current
        Neg a' -> neg (assert a' current)

verify :: Eq a => FormulaWith (Negation a) -> [a] -> Result
verify = verifyWith $ \a b -> fromBool (a == b)
