{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module WTP.Verify
  ( EvalError (..),
    eval,
    verify,
  )
where

import Algebra.Lattice (Lattice (..))
import Control.Lens hiding (op)
import Control.Monad.Except (MonadError (throwError))
import Data.Generics.Sum (AsConstructor, _Ctor)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Functor ((<&>))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Read as Text
import Data.Traversable (for)
import qualified Data.Vector as Vector
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import WTP.Formula
import WTP.Query
import WTP.Result
import WTP.Trace
import WTP.Value
import Data.Bool (bool)
import GHC.Generics (Generic)

data EvalError
  = TypeError Value Text
  | ApplyError Value [Value]
  | QueryError Query
  | RuntimeError Text
  | Undetermined
  deriving (Eq, Show, Generic)

type Eval = Either EvalError

require ::
  forall (ctor :: Symbol) s t a b.
  (KnownSymbol ctor, AsConstructor ctor s t a b, s ~ Value, t ~ Value, a ~ b) =>
  Proxy ctor ->
  Value ->
  Eval b
require (ctor :: Proxy ctor) v = case v ^? _Ctor @ctor of
  Just x -> pure x
  Nothing -> throwError (TypeError v (Text.toLower (Text.drop 1 (Text.pack (symbolVal ctor)))))

boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Eval Value
boolOp op' v1 v2 = do
  b1 <- require (Proxy @"VBool") v1
  b2 <- require (Proxy @"VBool") v2
  pure (VBool (op' b1 b2))

eval :: [ObservedState] -> Formula -> Eval Value
eval [] (Always _) = pure (VBool True)
eval [] _ = throwError Undetermined
eval steps@(current : rest) f = case f of
  Literal l -> pure l
  Set ps -> VSet . HashSet.fromList <$> traverse (eval steps) ps
  Seq ps -> VSeq . Vector.fromList <$> traverse (eval steps) ps
  Next p -> eval rest p
  Always p -> do
    first' <- case eval steps p of
      Right v -> pure v
      Left Undetermined -> pure (VBool True)
      Left e -> throwError e
    rest' <- (eval rest (Always p))
    boolOp (/\) first' rest'
  Apply (Literal (VFunction (BuiltInFunction FOr))) [a1, a2] ->
    case eval steps a1 of
      Right (VBool True) -> pure (VBool True)
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
      (VString s1, _) -> do
        s2 <- require (Proxy @"VString") q'
        pure (VBool (s1 `op` s2))
      (VNumber n1, _) -> do
        n2 <- require (Proxy @"VNumber") q'
        pure (VBool (n1 `op` n2))
      _ -> throwError (TypeError p' "string or number")
  BindQuery QueryAll query -> VSeq . Vector.fromList <$> evalQuery current query
  BindQuery QueryOne query ->
    evalQuery current query <&> \case
      (v : _) -> v
      [] -> VNull

evalFunc :: Value -> [Value] -> Eval Value
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
        vs' <- for (Vector.toList vs) $ \v -> do
          pass <- evalFunc f' (pure v) >>= require (Proxy @"VBool")
          if pass then pure (Vector.singleton v) else pure mempty
        pure (VSeq (Vector.concat vs'))
      (FMap, [f', VSeq vs]) -> VSeq <$> traverse (evalFunc f' . pure) vs
      (FHead, [v]) -> do
        vs <- require (Proxy @"VSeq") v
        if Vector.null vs then pure VNull else pure (Vector.head vs)
      (FTail, [v]) -> do
        vs <- require (Proxy @"VSeq") v
        if Vector.null vs then pure VNull else pure (VSeq (Vector.tail vs))
      (FInit, [v]) -> do
        vs <- require (Proxy @"VSeq") v
        if Vector.null vs then pure VNull else pure (VSeq (Vector.init vs))
      (FLast, [v]) -> do
        vs <- require (Proxy @"VSeq") v
        if Vector.null vs then pure VNull else pure (Vector.last vs)
      (FParseNumber, [VString s]) ->
        case Text.rational s of
          Left _ -> throwError (RuntimeError ("Cannot parse number: " <> s))
          Right (n, _) -> pure (VNumber n)
      (FSplitOn, [VString sep, VString s]) -> pure (VSeq (Vector.fromList (map VString (Text.splitOn sep s))))
      (FStrip, [v]) -> do
        s <- require (Proxy @"VString") v
        pure (VString (Text.strip s))
      _ -> throwError (ApplyError f args)
  _ -> throwError (ApplyError f args)

evalQuery :: ObservedState -> Query -> Eval [Value]
evalQuery (ObservedState current) query = maybe (throwError (QueryError query)) pure (HashMap.lookup query current)

verify :: [ObservedState] -> Formula -> Eval Result
verify trace formula =
  eval trace formula >>= require (Proxy @"VBool") <&> bool Rejected Accepted
