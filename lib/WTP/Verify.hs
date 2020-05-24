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

import Algebra.Lattice (Lattice (..))
import Control.Applicative (Alternative (..))
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import WTP.Formula
import WTP.Query
import WTP.Value
import WTP.Result
import WTP.Trace
import qualified Data.HashSet as HashSet
import Data.Traversable (for)
import qualified Data.Text.Read as Text
import qualified Data.Text as Text

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
  Next p -> eval rest p
  Always p -> do
    first' <- (eval steps p <|> Just (VBool True))
    rest' <- (eval rest (Always p))
    boolOp (/\) first' rest'

  Apply (Literal (VFunction (BuiltInFunction FOr))) [a1, a2] ->
    case eval steps a1 of
      Just (VBool True) -> pure (VBool True)
      _ -> eval steps a2
  Apply func args -> do
    func' <- eval steps func
    args' <- traverse (eval steps) args
    evalFunc func' args'
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
  BindQuery QueryOne query ->
    case evalQuery current query of
      Just (v : _) -> pure v
      _ -> pure VNull
    

evalFunc :: Value -> [Value] -> Maybe Value
evalFunc f args = case f of
  (VFunction (BuiltInFunction bif)) ->
    case (bif, args) of
      (FAnd, [VBool b1, VBool b2]) -> pure (VBool (b1 /\ b2))
      (FOr, [VBool b1, VBool b2]) -> pure (VBool (b1 \/ b2))
      (FNot, [VBool b]) -> pure (VBool (not b))
      (FIdentity, [arg]) -> pure arg
      (FLength, [VSeq vs]) -> pure (VNumber (fromIntegral (Vector.length vs)))
      (FLength, [VString s]) -> pure (VNumber (fromIntegral (Text.length s)))
      (FFilter, [f', VSeq vs]) -> do
        vs' <- for vs $ \v -> do
          evalFunc f' (pure v) >>= \case
            VBool True -> pure v
            _ -> Nothing
        pure (VSeq vs')
      (FMap, [f', VSeq vs]) -> VSeq <$> traverse (evalFunc f' . pure) vs
      (FHead, [VSeq vs])
        | not (Vector.null vs) -> pure (Vector.head vs)
      (FTail, [VSeq vs])
        | not (Vector.null vs) -> pure (VSeq (Vector.tail vs))
      (FInit, [VSeq vs])
        | not (Vector.null vs) -> pure (VSeq (Vector.init vs))
      (FLast, [VSeq vs])
        | not (Vector.null vs) -> pure (Vector.last vs)
      (FParseNumber, [VString s]) ->
        case Text.rational s of
          Left _ -> Nothing
          Right (n, _) -> pure (VNumber n)
      (FSplitOn, [VString sep, VString s]) -> pure (VSeq (Vector.fromList (map VString (Text.splitOn sep s))))
      (FStrip, [VString s]) -> pure (VString (Text.strip s))
      _ -> Nothing
  _ -> Nothing

evalQuery :: ObservedState -> Query -> Maybe [Value]
evalQuery (ObservedState current) query = HashMap.lookup query current

verify :: [ObservedState] -> Formula -> Result
verify trace formula = case (fromMaybe (VBool False) (eval trace formula)) of
  VBool True -> Accepted
  _ -> Rejected
