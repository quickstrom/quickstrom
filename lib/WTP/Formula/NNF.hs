{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Formula.NNF where

import Control.Monad.Freer
import WTP.Query
import WTP.Assertion
import Prelude hiding (False, True)

type IsQuery eff = Members '[Query] eff

data QueryAssertion where
  QueryAssertion :: Show a => Eff '[Query] a -> Assertion a -> QueryAssertion

instance Show QueryAssertion where
  show (QueryAssertion _q a) = "(QueryAssertion _ " <> show a <> ")"

  
data Negation a = Neg a | Pos a
  deriving (Show, Functor, Foldable, Traversable)

withAtomic :: (a -> b) -> Negation a -> b
withAtomic f (Neg a) = f a
withAtomic f (Pos a) = f a

data Formula
  = True
  | False
  | And Formula Formula
  | Or Formula Formula
  | Until Formula Formula
  | Release Formula Formula
  | Assert (Negation QueryAssertion)

withQueries :: Monad m => (forall a. Eff '[Query] a -> m b) -> Formula -> m [b]
withQueries f = \case
  True -> pure []
  False -> pure []
  And p q -> (<>) <$> withQueries f p <*> withQueries f q
  Or p q -> (<>) <$> withQueries f p <*> withQueries f q
  Until p q -> (<>) <$> withQueries f p <*> withQueries f q
  Assert a -> (: []) <$> withAtomic (\(QueryAssertion q _) -> f q) a

instance Show Formula where
  show = \case
    True -> "True"
    False -> "False"
    And p q -> "(And " <> show p <> " " <> show q <> ")"
    Or p q -> "(Or " <> show p <> " " <> show q <> ")"
    Until p q -> "(Until " <> show p <> " " <> show q <> ")"
    Assert qa -> "(Assert _ " <> show qa <> ")"
