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
import Algebra.Lattice (Lattice (..))
import Control.Applicative (Alternative (..))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Vector as Vector
import WTP.Formula
import WTP.Query
import WTP.Result
import WTP.Trace

boolOp :: (Bool -> Bool -> Bool) -> JSON.Value -> JSON.Value -> Maybe JSON.Value
boolOp op' (JSON.Bool b1) (JSON.Bool b2) = Just (JSON.Bool (op' b1 b2))
boolOp _ _ _ = Nothing

eval :: [ObservedState] -> Formula -> Maybe JSON.Value
eval [] (Always _) = pure (JSON.Bool True)
eval [] _ = Nothing
eval steps@(current : rest) f = case f of
  Literal l -> pure l
  Set ps -> JSON.Array . Vector.uniq . Vector.fromList <$> traverse (eval steps) ps
  Seq ps -> JSON.Array . Vector.fromList <$> traverse (eval steps) ps
  Not p -> case eval steps p of
    Just (JSON.Bool b) -> Just (JSON.Bool (neg b))
    _ -> Nothing
  p `And` q ->
    case (eval steps p, eval steps q) of
      (Just a, Just b) -> boolOp (/\) a b
      _ -> Nothing
  p `Or` q ->
    case eval steps p of
      Just (JSON.Bool True) -> pure (JSON.Bool True)
      _ -> eval steps q
  Next p -> eval rest p
  Always p -> do
    first' <- (eval steps p <|> Just (JSON.Bool True))
    rest' <- (eval rest (Always p))
    boolOp (/\) first' rest'
  Equals p q -> do
    p' <- eval steps p
    q' <- eval steps q
    pure (JSON.Bool (p' == q'))
  Compare comparison p q -> do
    let op :: Ord a => a -> a -> Bool
        op = case comparison of
          LessThan -> (<)
          LessThanEqual -> (<=)
          GreaterThan -> (>)
          GreaterThanEqual -> (>=)
    p' <- eval steps p
    q' <- eval steps q
    case (p', q') of
      (JSON.String s1, JSON.String s2) -> pure (JSON.Bool (s1 `op` s2))
      (JSON.Number n1, JSON.Number n2) -> pure (JSON.Bool (n1 `op` n2))
      _ -> Nothing
  BindQuery QueryAll query -> JSON.Array . Vector.fromList <$> evalQuery current query
  BindQuery QueryOne query -> evalQuery current query >>= listToMaybe

evalQuery :: ObservedState -> Query -> Maybe [JSON.Value]
evalQuery (ObservedState current) query = HashMap.lookup query current

verify :: [ObservedState] -> Formula -> Result
verify trace formula = case (fromMaybe (JSON.Bool False) (eval trace formula)) of
  JSON.Bool True -> Accepted
  _ -> Rejected
