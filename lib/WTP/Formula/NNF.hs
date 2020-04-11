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

import Control.Monad.Freer
import WTP.Assertion
import WTP.Query
import Prelude hiding (False, True)

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
  Assert a -> (: []) <$> withAtomic (\(QueryAssertion q _) -> f q) a
