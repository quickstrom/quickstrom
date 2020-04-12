{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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

module WTP.Formula.Minimal where

import Algebra.Lattice
import Control.Monad.Freer
import WTP.Assertion
import WTP.Query
import WTP.Result
import Prelude hiding (False, True)
import Algebra.Heyting (Heyting(neg))

type IsQuery eff = Members '[Query] eff

data FormulaWith assertion
  = True
  | Not (FormulaWith assertion)
  | Or (FormulaWith assertion) (FormulaWith assertion)
  | Until (FormulaWith assertion) (FormulaWith assertion)
  | Assert assertion
  deriving (Show)

type Formula = FormulaWith QueryAssertion

withQueries :: Monad m => (forall a. Eff '[Query] a -> m b) -> Formula -> m [b]
withQueries f = \case
  True -> pure []
  Not p -> withQueries f p
  Or p q -> (<>) <$> withQueries f p <*> withQueries f q
  Until p q -> (<>) <$> withQueries f p <*> withQueries f q
  Assert (QueryAssertion q _) -> (: []) <$> f q

verifyWith :: (a -> step -> Result) -> FormulaWith a -> [step] -> Result
verifyWith assert = go
  where
    go spec steps =
      case steps of
        [] -> Accepted
        current : rest ->
          case spec of
            True -> Accepted
            Not p -> neg (go p steps)
            p `Or` q -> go p steps \/ go q steps
            p `Until` q ->
              case (go p [current], go q rest) of
                (_, Accepted) -> Accepted
                (Accepted, Rejected) -> go (p `Until` q) rest
                (r1, _) -> r1
            Assert a -> assert a current

verify :: Eq a => FormulaWith a -> [a] -> Result
verify = verifyWith $ \a b -> fromBool (a == b)
