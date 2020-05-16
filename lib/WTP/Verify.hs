{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module WTP.Verify
  ( eval,
    verify,
  )
where

import Control.Lens ((%~), (^?))
import Algebra.Heyting (Heyting (..))
import Algebra.Lattice (bottom, fromBool)
import Prelude
import Control.Applicative (Alternative (..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Generics.Sum (_Ctor)
import Data.Maybe (listToMaybe, fromMaybe)
import WTP.Core.Formula
import WTP.Core.Query
import WTP.Core.Value
import WTP.Result
import WTP.Trace

eval :: [ObservedState] -> Formula -> Maybe Value
eval [] (Always _) = Just (VBoolean True)
eval [] _ = Nothing
eval steps@(current : rest) f = case f of
  Literal v -> pure v
  Set ps -> VSet . HashSet.fromList <$> traverse (eval steps) ps
  Seq ps -> VSeq <$> traverse (eval steps) ps
  Not p -> ((_Ctor @"VBoolean") %~ neg) <$> eval steps p 
  p `And` q -> eval steps p `vand` eval steps q
  p `Or` q ->
    case eval steps p of
      Just (VBoolean True) -> pure (VBoolean True)
      _ -> eval steps q
  Next p -> eval rest p
  Always p -> (eval steps p <|> Just (VBoolean True)) `vand` (eval rest (Always p))
  Equals p q -> do
    p' <- eval steps p
    q' <- eval steps q
    pure (VBoolean (p' == q'))
  Compare comparison p q -> do
    let op = case comparison of
          LessThan -> (<)
          LessThanEqual -> (<=)
          GreaterThan -> (>)
          GreaterThanEqual -> (>=)
    p' <- eval steps p
    q' <- eval steps q
    VBoolean . (`op` EQ) <$> (p' `compareWith` q')
  QueryAll query -> VSeq <$> evalQuery current query
  QueryOne query -> evalQuery current query >>= listToMaybe
  -- MapFormula fn sub -> fn <$> eval steps sub

valueToBool :: Value -> Maybe Bool
valueToBool = (^? _Ctor @"VBoolean")

vand :: Maybe Value -> Maybe Value -> Maybe Value
vand v1 v2 = do
  v1 >>= valueToBool >>= \case
    True -> v2
    False -> pure (VBoolean False)

evalQuery :: ObservedState -> Query -> Maybe [Value]
evalQuery current query = go query
  where
    go :: Query -> Maybe [Value]
    go = \case
      Get state sub ->
        go sub >>= mapM \v -> do
          el <- v ^? _Ctor @"VElement"
          let states = fromMaybe mempty (HashMap.lookup el (elementStates current))
           in findElementState state states
      ByCss selector ->
        case HashMap.lookup selector (queriedElements current) of
          Just es -> pure (map VElement es)
          _ -> pure mempty

verify :: [ObservedState] -> Formula -> Result
verify trace formula = fromBool (fromMaybe bottom (valueToBool =<< eval trace formula))
