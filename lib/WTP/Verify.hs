{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module WTP.Verify
  ( eval,
    verify,
  )
where

import Algebra.Heyting (Heyting (..))
import Algebra.Lattice (Lattice (..), bottom, fromBool, top)
import Control.Applicative (Alternative (..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Type.Reflection
import WTP.Formula
import WTP.Query
import WTP.Result
import WTP.Trace

eval :: [ObservedState] -> Formula a -> Maybe a
eval [] (Always _) = Just True
eval [] _ = Nothing
eval steps@(current : rest) f = case f of
  Literal LTrue -> pure top
  Literal LFalse -> pure bottom
  Literal (LNum n) -> pure n
  Literal (LString t) -> pure t
  Literal (LJson j) -> pure j
  Set ps -> HashSet.fromList <$> traverse (eval steps) ps
  Seq ps -> traverse (eval steps) ps
  Not p -> neg <$> eval steps p
  p `And` q ->
    case (eval steps p, eval steps q) of
      (Just a, Just b) -> pure (a /\ b)
      _ -> Nothing
  p `Or` q ->
    case eval steps p of
      Just True -> pure True
      _ -> eval steps q
  Next p -> eval rest p
  Always p -> (/\) <$> (eval steps p <|> Just True) <*> (eval rest (Always p))
  Equals p q -> do
    p' <- eval steps p
    q' <- eval steps q
    pure (fromBool (p' == q'))
  Compare comparison p q -> do
    let op = case comparison of
          LessThan -> (<)
          LessThanEqual -> (<=)
          GreaterThan -> (>)
          GreaterThanEqual -> (>=)
    p' <- eval steps p
    q' <- eval steps q
    pure (fromBool (p' `op` q'))
  BindQuery query -> evalQuery current query
  MapFormula fn sub -> fn <$> eval steps sub

evalQuery :: (Show a, Eq a, Hashable a, Typeable a) => ObservedState -> Query a -> Maybe [a]
evalQuery (ObservedState current) (query :: Query a) =
  traverse (castValue (typeRep @a)) =<< HashMap.lookup (SomeQuery query) current

verify :: [ObservedState] -> Proposition -> Result
verify trace formula = fromBool (fromMaybe bottom (eval trace formula))
