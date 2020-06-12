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

module WebCheck.PureScript.ForeignFunction where

import Data.HashMap.Strict (HashMap)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Language.PureScript.AST (SourceSpan)
import Language.PureScript.Names
import Protolude hiding (Selector)
import WebCheck.Action (Action(..))
import WebCheck.Element (Selector (..))
import WebCheck.PureScript.Eval.Class
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Value
import WebCheck.Path

data ForeignFunction m ann
  = ForeignFunction
      { arity :: Int,
        evalFn :: (SourceSpan -> [Value ann] -> m (Value ann))
      }

foreignFunction :: (MonadError EvalError m, ForeignFunctionArity f, EvalForeignFunction m ann f) => f -> ForeignFunction m ann
foreignFunction (f :: f) =
  ForeignFunction
    (foreignFunctionArity (Proxy :: Proxy f))
    (evalForeignFunction f)

class ForeignFunctionArity f where
  foreignFunctionArity :: Proxy f -> Int

instance {-# OVERLAPPABLE #-} ForeignFunctionArity a where
  foreignFunctionArity _ = 0

instance {-# OVERLAPPING #-} ForeignFunctionArity b => ForeignFunctionArity (a -> b) where
  foreignFunctionArity (_ :: Proxy (a -> b)) = succ (foreignFunctionArity (Proxy :: Proxy b))

foreignFunctionArityMismatch :: MonadError EvalError m => SourceSpan -> m a
foreignFunctionArityMismatch ss = throwError (ForeignFunctionError (Just ss) "Foreign function arity mismatch")

type family BaseCase a where
  BaseCase (a -> b) = 'False
  BaseCase a = 'True

class (Monad m, MonadError EvalError m) => EvalForeignFunction m ann f where
  evalForeignFunction :: f -> SourceSpan -> [Value ann] -> m (Value ann)

instance {-# OVERLAPPABLE #-} (MonadError EvalError m, FromHaskellValue a) => EvalForeignFunction m ann a where
  evalForeignFunction x _ [] = pure (fromHaskellValue x)
  evalForeignFunction _ ss _ = foreignFunctionArityMismatch ss

instance {-# OVERLAPPABLE #-} (MonadError EvalError m, FromHaskellValue a) => EvalForeignFunction m ann (m a) where
  evalForeignFunction x _ [] = fromHaskellValue <$> x
  evalForeignFunction _ ss _ = foreignFunctionArityMismatch ss

instance {-# OVERLAPPING #-} (ToHaskellValue a ann, EvalForeignFunction m ann b) => EvalForeignFunction m ann (a -> b)  where
  evalForeignFunction f ss (v : vs) = do
    a <- toHaskellValue ss v
    evalForeignFunction (f a) ss vs
  evalForeignFunction _ ss [] = foreignFunctionArityMismatch ss

class ToHaskellValue r ann where
  toHaskellValue :: MonadError EvalError m => SourceSpan -> Value ann -> m r

instance ToHaskellValue (Value ann) ann where
  toHaskellValue _ = pure

instance ToHaskellValue Bool ann where
  toHaskellValue ss = require ss (Proxy @"VBool")

instance ToHaskellValue Text ann where
  toHaskellValue ss = require ss (Proxy @"VString")

instance ToHaskellValue Char ann where
  toHaskellValue ss = require ss (Proxy @"VChar")

instance ToHaskellValue Int ann where
  toHaskellValue ss = require ss (Proxy @"VInt")

instance ToHaskellValue Double ann where
  toHaskellValue ss = require ss (Proxy @"VNumber")

instance ToHaskellValue a ann => ToHaskellValue (Vector a) ann where
  toHaskellValue ss = traverse (toHaskellValue ss) <=< require ss (Proxy @"VArray")

instance ToHaskellValue a ann => ToHaskellValue [a] ann where
  toHaskellValue ss x = Vector.toList <$> toHaskellValue ss x

instance ToHaskellValue a ann => ToHaskellValue (HashMap Text a) ann where
  toHaskellValue ss = traverse (toHaskellValue ss) <=< require ss (Proxy @"VObject")

instance ToHaskellValue (Action Selector) ann where
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

instance (MonadEval m, ann ~ Ann m, FromHaskellValue a, ToHaskellValue b ann) => ToHaskellValue (a -> m b) ann where
  toHaskellValue ss fn =
    pure
      ( \x -> do
          fn' <- require ss (Proxy @"VFunction") fn
          b <- evalFunc fn' (fromHaskellValue x)
          toHaskellValue ss b
      )

instance (MonadEval m, ann ~ Ann m, FromHaskellValue a, FromHaskellValue b, ToHaskellValue c ann) => ToHaskellValue (a -> b -> m c) ann where
  toHaskellValue ss fn = do
    pure
      ( \a b -> do
          fn' <- require ss (Proxy @"VFunction") fn
          fn'' <- require ss (Proxy @"VFunction") =<< evalFunc fn' (fromHaskellValue a)
          c <- evalFunc fn'' (fromHaskellValue b)
          toHaskellValue ss c
      )

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

foreignFunctions :: MonadError EvalError m => Map (Qualified Ident) (ForeignFunction m ann)
foreignFunctions =
  Map.fromList
    [ (qualifiedName "Control.Bind" "arrayBind", foreignFunction arrayBind)
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
    ]
  where
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
--arrayMap :: (a ~ Value EvalAnn, b ~ Value EvalAnn) => (a -> Eval b) -> Vector a -> Eval (Vector b)
--arrayMap = Vector.mapM
--arrayBind :: (a ~ Value EvalAnn, b ~ Value EvalAnn) => Vector a -> (a -> Eval (Vector b)) -> Eval (Vector b)
--arrayBind xs f = join <$> traverse f xs
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
