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
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Vector as Vector
import WTP.Formula
import WTP.Query
import WTP.Value
import WTP.Result
import WTP.Trace
import qualified Data.HashSet as HashSet

boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Maybe Value
boolOp op' (VBool b1) (VBool b2) = Just (VBool (op' b1 b2))
boolOp _ _ _ = Nothing

eval :: [ObservedState] -> Formula -> Maybe Value
eval [] (Always _) = pure (VBool True)
eval [] _ = Nothing
eval steps@(current : rest) f = case f of
  Literal l -> pure l
  Set ps -> VSet . HashSet.fromList <$> traverse (eval steps) ps
  Seq ps -> VSeq . Vector.fromList <$> traverse (eval steps) ps
  Not p -> case eval steps p of
    Just (VBool b) -> Just (VBool (neg b))
    _ -> Nothing
  p `And` q ->
    case (eval steps p, eval steps q) of
      (Just a, Just b) -> boolOp (/\) a b
      _ -> Nothing
  p `Or` q ->
    case eval steps p of
      Just (VBool True) -> pure (VBool True)
      _ -> eval steps q
  Next p -> eval rest p
  Always p -> do
    first' <- (eval steps p <|> Just (VBool True))
    rest' <- (eval rest (Always p))
    boolOp (/\) first' rest'
  Equals p q -> do
    p' <- eval steps p
    q' <- eval steps q
    pure (VBool (p' == q'))
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
      (VString s1, VString s2) -> pure (VBool (s1 `op` s2))
      (VNumber n1, VNumber n2) -> pure (VBool (n1 `op` n2))
      _ -> Nothing
  BindQuery QueryAll query -> VSeq . Vector.fromList <$> evalQuery current query
  BindQuery QueryOne query -> evalQuery current query >>= listToMaybe

evalQuery :: ObservedState -> Query -> Maybe [Value]
evalQuery (ObservedState current) query = HashMap.lookup query current

verify :: [ObservedState] -> Formula -> Result
verify trace formula = case (fromMaybe (VBool False) (eval trace formula)) of
  VBool True -> Accepted
  _ -> Rejected
