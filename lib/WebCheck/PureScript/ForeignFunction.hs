{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module WebCheck.PureScript.ForeignFunction where

import Data.HashMap.Strict (HashMap)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.TypeNats (type (+))
import Language.PureScript.AST (SourceSpan)
import Language.PureScript.Names
import Protolude hiding (Selector)
import WebCheck.Action (Action (..))
import WebCheck.Element (Selector (..))
import WebCheck.Path
import WebCheck.PureScript.Eval.Class
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Value

data ForeignFunction m
  = ForeignFunction
      { arity :: Int,
        evalFn :: (SourceSpan -> [Value (Ann m)] -> m (Value (Ann m)))
      }

-- class ForeignFunctionArity' (baseCase :: Bool) f where
--   foreignFunctionArity' :: Proxy baseCase -> Proxy f -> Int
--
-- instance ForeignFunctionArity' (BaseCase b) b => ForeignFunctionArity' 'False (a -> b) where
--   foreignFunctionArity' _ (_ :: Proxy (a -> b)) = succ (foreignFunctionArity (Proxy :: Proxy b))
--
-- instance ForeignFunctionArity' 'True a where
--   foreignFunctionArity' _ = 0
--
-- type ForeignFunctionArity a = ForeignFunctionArity' (BaseCase a) a

type family FunctionArity (f :: *) where
  FunctionArity (a -> b) = FunctionArity b + 1
  FunctionArity a = 0

type family IsBaseCase (f :: *) where
  IsBaseCase (a -> b) = 'False
  IsBaseCase a = 'True

functionArity :: (KnownNat (FunctionArity f)) => Proxy f -> Int
functionArity (_ :: Proxy f) = fromIntegral (natVal (Proxy :: Proxy (FunctionArity f)))

foreignFunctionArityMismatch :: MonadError EvalError m => SourceSpan -> m a
foreignFunctionArityMismatch ss = throwError (ForeignFunctionError (Just ss) "Foreign function arity mismatch")

newtype Ret a = Ret {unRet :: a}

class EvalForeignFunction' m (b :: Bool) f where
  evalForeignFunction' :: Proxy b -> f -> SourceSpan -> [Value (Ann m)] -> m (Value (Ann m))

instance
  -- {-# OVERLAPPABLE #-}
  ( MonadError EvalError m,
    FromHaskellValue a,
    f ~ m (Ret a)
  ) =>
  EvalForeignFunction' m 'True f
  where
  evalForeignFunction' _ x _ [] = fromHaskellValue . unRet <$> x
  evalForeignFunction' _ _ ss _ = foreignFunctionArityMismatch ss

instance
  -- {-# OVERLAPPING #-}
  ( MonadError EvalError m,
    ToHaskellValue m a,
    EvalForeignFunction' m (IsBaseCase b) (m b),
    m ~ n
  ) =>
  EvalForeignFunction' m 'False (a -> n b)
  where
  evalForeignFunction' _ f ss (v : vs) = do
    a <- toHaskellValue ss v
    evalForeignFunction' (Proxy :: Proxy (IsBaseCase b)) (f a) ss vs
  evalForeignFunction' _ _ ss [] = foreignFunctionArityMismatch ss

class EvalForeignFunction m f where
  evalForeignFunction :: f -> SourceSpan -> [Value (Ann m)] -> m (Value (Ann m))

instance (EvalForeignFunction' m (IsBaseCase f) f) => EvalForeignFunction m f where
  evalForeignFunction = evalForeignFunction' (Proxy :: Proxy (IsBaseCase f))

-- instance {-# OVERLAPPABLE #-} (MonadError EvalError m, FromHaskellValue a) => EvalForeignFunction m ann a where
--   evalForeignFunction x _ [] = pure (fromHaskellValue x)
--   evalForeignFunction _ ss _ = foreignFunctionArityMismatch ss

class MonadError EvalError m => ToHaskellValue m r where
  toHaskellValue :: SourceSpan -> Value (Ann m) -> m r

instance (MonadError EvalError m, ann ~ Ann m) => ToHaskellValue m (Value ann) where
  toHaskellValue _ = pure

instance MonadError EvalError m => ToHaskellValue m Bool where
  toHaskellValue ss = require ss (Proxy @"VBool")

instance MonadError EvalError m => ToHaskellValue m Text where
  toHaskellValue ss = require ss (Proxy @"VString")

instance MonadError EvalError m => ToHaskellValue m Char where
  toHaskellValue ss = require ss (Proxy @"VChar")

instance MonadError EvalError m => ToHaskellValue m Int where
  toHaskellValue ss = require ss (Proxy @"VInt")

instance MonadError EvalError m => ToHaskellValue m Double where
  toHaskellValue ss = require ss (Proxy @"VNumber")

instance ToHaskellValue m a => ToHaskellValue m (Vector a) where
  toHaskellValue ss = traverse (toHaskellValue ss) <=< require ss (Proxy @"VArray")

instance ToHaskellValue m a => ToHaskellValue m [a] where
  toHaskellValue ss x = Vector.toList <$> toHaskellValue ss x

instance ToHaskellValue m a => ToHaskellValue m (HashMap Text a) where
  toHaskellValue ss = traverse (toHaskellValue ss) <=< require ss (Proxy @"VObject")

instance MonadError EvalError m => ToHaskellValue m (Action Selector) where
  toHaskellValue ss v = do
    obj <- require ss (Proxy @"VObject") v
    ctor <- require ss (Proxy @"VString") =<< accessField ss "constructor" obj
    value <- Vector.head <$> (require ss (Proxy @"VArray") =<< accessField ss "fields" obj)
    case ctor of
      "Focus" -> Focus . Selector <$> toHaskellValue ss value
      "KeyPress" -> KeyPress <$> toHaskellValue ss value
      "Click" -> Click . Selector <$> toHaskellValue ss value
      "Navigate" -> Navigate . Path <$> toHaskellValue ss value
      _ -> throwError (ForeignFunctionError (Just ss) ("Unknown Action constructor: " <> ctor))

instance (MonadEval m, FromHaskellValue a, ToHaskellValue m b) => ToHaskellValue m (a -> m b) where
  toHaskellValue ss fn =
    pure
      ( \x -> do
          fn' <- require ss (Proxy @"VFunction") fn
          b <- evalFunc fn' (fromHaskellValue x)
          toHaskellValue ss b
      )

{-
instance (MonadEval m, ann ~ Ann m, FromHaskellValue a, FromHaskellValue b, ToHaskellValue c ann) => ToHaskellValue (a -> b -> m c) ann where
  toHaskellValue ss fn = do
    pure
      ( \a b -> do
          fn' <- require ss (Proxy @"VFunction") fn
          fn'' <- require ss (Proxy @"VFunction") =<< evalFunc fn' (fromHaskellValue a)
          c <- evalFunc fn'' (fromHaskellValue b)
          toHaskellValue ss c
      )
-}

class FromHaskellValue a where
  fromHaskellValue :: a -> Value ann

instance FromHaskellValue Bool where
  fromHaskellValue = VBool

instance FromHaskellValue Int where
  fromHaskellValue = VInt

instance FromHaskellValue Double where
  fromHaskellValue = VNumber

instance FromHaskellValue Char where
  fromHaskellValue = VChar

instance FromHaskellValue Text where
  fromHaskellValue = VString

instance FromHaskellValue a => FromHaskellValue (Vector a) where
  fromHaskellValue xs = VArray (fromHaskellValue <$> xs)

instance FromHaskellValue a => FromHaskellValue [a] where
  fromHaskellValue = fromHaskellValue . Vector.fromList

-- instance FromHaskellValue (Value ann) where
--   fromHaskellValue = identity

qualifiedName :: Text -> Text -> Qualified Ident
qualifiedName mn n = Qualified (Just (ModuleName mn)) (Ident n)

foreignFunction ::
  (KnownNat (FunctionArity f), EvalForeignFunction' m (IsBaseCase f) f) =>
  Proxy m ->
  f ->
  ForeignFunction m
foreignFunction _ (f :: f) =
  ForeignFunction
    (functionArity (Proxy :: Proxy f))
    (evalForeignFunction' (Proxy :: Proxy (IsBaseCase f)) f)

-- foo :: MonadEval m => ForeignFunction m
-- foo = ForeignFunction 2 (evalForeignFunction arrayBind)

arrayBind :: (MonadEval m, a ~ Value (Ann m), b ~ Value (Ann m)) => Vector a -> (a -> m (Vector b)) -> m (Vector b)
arrayBind xs f = join <$> traverse f xs

bar :: MonadEval m => Proxy m -> ForeignFunction m
bar (Proxy :: Proxy m) = ForeignFunction 0 (evalForeignFunction bar2 :: SourceSpan -> [Value (Ann m)] -> m (Value (Ann m)))

bar1 :: Monad m => m (Ret Int)
bar1 = pure (Ret 1)

bar2 :: Monad m => Int -> m (Ret Int)
bar2 n = pure (Ret (succ n))

foreignFunctions :: MonadEval m => Map (Qualified Ident) (ForeignFunction m)
foreignFunctions =
  Map.fromList
    []
  where

-- (qualifiedName "Control.Bind" "arrayBind", foreignFunction arrayBind)
-- ,
--   (qualifiedName "Data.Array" "indexImpl", foreignFunction indexImpl),
--   (qualifiedName "Data.Array" "length", foreignFunction len),
--   (qualifiedName "Data.Array" "filter", foreignFunction filterArray),
--   (qualifiedName "Data.Bounded" "bottomInt", foreignFunction (op0 @Int minBound)),
--   (qualifiedName "Data.Bounded" "topInt", foreignFunction (op0 @Int maxBound)),
--   (qualifiedName "Data.Eq" "eqBooleanImpl", foreignFunction (op2 ((==) @Bool))),
--   (qualifiedName "Data.Eq" "eqIntImpl", foreignFunction (op2 ((==) @Int))),
--   (qualifiedName "Data.Eq" "eqNumberImpl", foreignFunction (op2 ((==) @Double))),
--   (qualifiedName "Data.Eq" "eqCharImpl", foreignFunction (op2 ((==) @Char))),
--   (qualifiedName "Data.Eq" "eqStringImpl", foreignFunction (op2 ((==) @Text))),
--   (qualifiedName "Data.Eq" "eqArrayImpl", foreignFunction eqArray),
--   (qualifiedName "Data.EuclideanRing" "intDegree", foreignFunction intDegree),
--   (qualifiedName "Data.EuclideanRing" "intDiv", foreignFunction intDiv),
--   (qualifiedName "Data.EuclideanRing" "intMod", foreignFunction intMod),
--   (qualifiedName "Data.EuclideanRing" "numDiv", foreignFunction (op2 @Double (/))),
--   (qualifiedName "Data.Foldable" "foldlArray", foreignFunction foldlArray),
--   (qualifiedName "Data.Foldable" "foldrArray", foreignFunction foldrArray),
--   (qualifiedName "Data.Functor" "arrayMap", foreignFunction arrayMap),
--   (qualifiedName "Data.HeytingAlgebra" "boolConj", foreignFunction (op2 (&&))),
--   (qualifiedName "Data.HeytingAlgebra" "boolDisj", foreignFunction (op2 (||))),
--   (qualifiedName "Data.HeytingAlgebra" "boolNot", foreignFunction (op1 not)),
--   (qualifiedName "Data.Int" "toNumber", foreignFunction (op1 (fromIntegral @Int @Double))),
--   (qualifiedName "Data.Int" "fromNumberImpl", foreignFunction fromNumberImpl),
--   (qualifiedName "Data.Int" "fromStringAsImpl", foreignFunction fromStringAsImpl),
--   (qualifiedName "Data.Ord" "ordBooleanImpl", foreignFunction (ordImpl @Bool)),
--   (qualifiedName "Data.Ord" "ordIntImpl", foreignFunction (ordImpl @Int)),
--   (qualifiedName "Data.Ord" "ordNumberImpl", foreignFunction (ordImpl @Double)),
--   (qualifiedName "Data.Ord" "ordStringImpl", foreignFunction (ordImpl @Text)),
--   (qualifiedName "Data.Ord" "ordCharImpl", foreignFunction (ordImpl @Char)),
--   (qualifiedName "Data.Ring" "intSub", foreignFunction (op2 ((-) @Int))),
--   (qualifiedName "Data.Ring" "numSub", foreignFunction (op2 ((-) @Double))),
--   (qualifiedName "Data.Show" "showStringImpl", foreignFunction (op1 (show @Text @Text))),
--   (qualifiedName "Data.Show" "showIntImpl", foreignFunction (op1 (show @Int @Text))),
--   (qualifiedName "Data.Show" "showNumberImpl", foreignFunction (op1 (show @Double @Text))),
--   (qualifiedName "Data.Show" "cons", foreignFunction (op2 (Vector.cons @(Value EvalAnn)))),
--   (qualifiedName "Data.Show" "join", foreignFunction (op2 Text.intercalate)),
--   (qualifiedName "Data.Semiring" "intAdd", foreignFunction (op2 ((+) @Int))),
--   (qualifiedName "Data.Semiring" "intMul", foreignFunction (op2 ((*) @Int))),
--   (qualifiedName "Data.Semiring" "numAdd", foreignFunction (op2 ((+) @Double))),
--   (qualifiedName "Data.Semiring" "numMul", foreignFunction (op2 ((*) @Double))),
--   (qualifiedName "Data.Semigroup" "concatString", foreignFunction (op2 ((<>) @Text))),
--   (qualifiedName "Data.Semigroup" "concatArray", foreignFunction (op2 ((<>) @(Vector (Value EvalAnn))))),
--   notSupported (qualifiedName "Data.String.Common" "_localeCompare"),
--   (qualifiedName "Data.String.Common" "replace", foreignFunction (op3 Text.replace)),
--   (qualifiedName "Data.String.Common" "split", foreignFunction (op2 Text.splitOn)),
--   (qualifiedName "Data.String.Common" "toLower", foreignFunction (op1 Text.toLower)),
--   (qualifiedName "Data.String.Common" "toUpper", foreignFunction (op1 Text.toUpper)),
--   (qualifiedName "Data.String.Common" "trim", foreignFunction (op1 Text.strip)),
--   (qualifiedName "Data.String.Common" "joinWith", foreignFunction (op2 Text.intercalate)),
--   (qualifiedName "Data.Unfoldable" "unfoldrArrayImpl", foreignFunction unfoldrArrayImpl),
--   (qualifiedName "Global" "infinity", foreignFunction (op0 (read "Infinity" :: Double))),
--   (qualifiedName "Global" "nan", foreignFunction (op0 (read "NaN" :: Double))),
--   (qualifiedName "Global" "isFinite", foreignFunction (op1 (not . isInfinite @Double))),
--   (qualifiedName "Global" "readFloat", foreignFunction (readAs Text.double)),
--   (qualifiedName "Global" "readInt", foreignFunction readInt),
--   (qualifiedName "Math" "floor", foreignFunction (op1 (fromIntegral @Int @Double . floor @Double @Int))),
--   (qualifiedName "Math" "remainder", foreignFunction (op2 (mod' @Double))),
--   (qualifiedName "Partial.Unsafe" "unsafePartial", foreignFunction unsafePartial),
--   (qualifiedName "Record.Unsafe" "unsafeGet", foreignFunction (accessField nullSourceSpan))

--notSupported :: MonadError (EvalError EvalAnn) m => Qualified Ident -> (Qualified Ident, ForeignFunction m)
--notSupported qn = (qn, ForeignFunction 0 (\ss _params -> throwError (ForeignFunctionNotSupported ss qn)))
--indexImpl :: a ~ (Value EvalAnn) => (a -> Eval (Value EvalAnn)) -> Value EvalAnn -> Vector a -> Int -> Eval (Value EvalAnn)
--indexImpl just nothing xs i = maybe (pure nothing) just (xs ^? ix (fromIntegral i))
--fromNumberImpl :: (Int -> Eval (Value EvalAnn)) -> Value EvalAnn -> Double -> Eval (Value EvalAnn)
--fromNumberImpl just _ = just . round
--fromStringAsImpl :: (Int -> Eval (Value EvalAnn)) -> Value EvalAnn -> Int -> Text -> Eval (Value EvalAnn)
--fromStringAsImpl just nothing radix t =
--  either (const (pure nothing)) (just . fst) $ case radix of
--    10 -> Text.decimal t
--    16 -> Text.hexadecimal t
--    _ -> Left mempty
--len :: Vector (Value EvalAnn) -> Eval Int
--len xs = pure (fromIntegral (Vector.length xs))
--filterArray :: (Value EvalAnn -> Eval Bool) -> Vector (Value EvalAnn) -> Eval (Vector (Value EvalAnn))
--filterArray = Vector.filterM
-- arrayBind :: MonadEval m => (a ~ Value ann, b ~ Value ann) => Vector a -> (a -> m (Vector b)) -> m (Vector b)
-- arrayBind xs f = join <$> traverse f xs
--arrayMap :: (a ~ Value EvalAnn, b ~ Value EvalAnn) => (a -> Eval b) -> Vector a -> Eval (Vector b)
--arrayMap = Vector.mapM
--foldlArray :: (b ~ Value EvalAnn, a ~ Value EvalAnn) => (b -> a -> Eval b) -> b -> Vector a -> Eval b
--foldlArray = foldM
--foldrArray :: (b ~ Value EvalAnn, a ~ Value EvalAnn) => (a -> b -> Eval b) -> b -> Vector a -> Eval b
--foldrArray = foldrM
--op0 :: forall a m. a -> Eval a
--op0 = pure
--op1 :: forall a b m. (a -> b) -> a -> Eval b
--op1 op = pure . op
--op2 :: forall a b c m. (a -> b -> c) -> a -> b -> Eval c
--op2 op x y = pure (op x y)
--op3 :: forall a b c d m. (a -> b -> c -> d) -> a -> b -> c -> Eval d
--op3 op x y z = pure (op x y z)
--readAs :: (StringConv s Text) => (Text -> Either s (a, Text)) -> Text -> Eval a
--readAs parse t = either (throwError . ForeignFunctionError Nothing . toS) (pure . fst) (parse t)
--readInt :: Int -> Text -> Eval Int
--readInt = \case
--  10 -> readAs Text.decimal
--  16 -> readAs Text.hexadecimal
--  radix -> const (throwError (ForeignFunctionError Nothing ("Unsupported radix for readInt: " <> show radix)))
--eqArray :: (a ~ Value EvalAnn, b ~ Bool) => (a -> a -> Eval b) -> Vector a -> Vector a -> Eval b
--eqArray pred' v1 v2
--  | Vector.length v1 == Vector.length v2 = Vector.and <$> Vector.zipWithM pred' v1 v2
--  | otherwise = pure False
--ordImpl :: forall a o. (Show a, Ord a, o ~ Value EvalAnn) => o -> o -> o -> a -> a -> Eval o
--ordImpl lt eq gt x y = pure $ case x `compare` y of
--  LT -> lt
--  EQ -> eq
--  GT -> gt
--intDegree :: Int -> Eval Int
--intDegree n = pure (min (abs n) 2147483647)
--intDiv :: Int -> Int -> Eval Int
--intDiv x y
--  | y == 0 = pure 0
--  | otherwise = pure (x `div` y)
--intMod :: Int -> Int -> Eval Int
--intMod x y
--  | y == 0 = pure 0
--  | otherwise = let yy = abs y in pure ((x `mod` yy) + yy `mod` yy)
--unfoldrArrayImpl ::
--  (Value EvalAnn -> Eval Bool) -> -- isNothing
--  (Value EvalAnn -> Eval (Value EvalAnn)) -> -- fromJust
--  (Value EvalAnn -> Eval (Value EvalAnn)) -> -- fst
--  (Value EvalAnn -> Eval (Value EvalAnn)) -> -- snd
--  (Value EvalAnn -> Eval (Value EvalAnn)) -> -- f
--  Value EvalAnn -> -- b
--  Eval (Vector (Value EvalAnn))
--unfoldrArrayImpl isNothing' fromJust' fst' snd' f =
--  Vector.unfoldrM $ \b -> do
--    r <- f b
--    isNothing' r >>= \case
--      True -> pure Nothing
--      False -> do
--        tuple <- fromJust' r
--        a <- fst' tuple
--        b' <- snd' tuple
--        pure (Just (a, b'))
--unsafePartial :: Value EvalAnn -> Eval (Value EvalAnn)
--unsafePartial f = do
--  Function fenv _ body <- require nullSourceSpan (Proxy @"VFunction") f
--  withModifiedEnv (const fenv) (eval body)