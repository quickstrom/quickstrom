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

import Algebra.Heyting (Heyting (..))
import Algebra.Lattice (Lattice (..), bottom, fromBool, top)
import Control.Applicative (Alternative (..))
import Control.Monad.Freer (Eff, reinterpret, runM, sendM, type (~>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Maybe (fromMaybe)
import WTP.Formula
import WTP.Query
import WTP.Result
import WTP.Trace

eval :: [ObservedState] -> Formula a -> Maybe a
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
  Always p -> (/\) <$> eval steps p <*> (eval rest (Always p) <|> Just True)
  Equals p q -> do
    p' <- eval steps p
    q' <- eval steps q
    pure (fromBool (p' == q'))
  BindQuery query -> evalQuery current query
  MapFormula fn sub -> fn <$> eval steps sub

evalQuery :: ObservedState -> Query a -> Maybe a
evalQuery current (Query eff) = runM (reinterpret go eff)
  where
    go :: QueryF ~> Eff '[Maybe]
    go = \case
      Get state el -> do
        let states = fromMaybe mempty (HashMap.lookup el (elementStates current))
         in sendM (findElementState state states)
      QueryAll selector ->
        case HashMap.lookup selector (queriedElements current) of
          Just es -> pure es
          _ -> pure mempty

verify :: [ObservedState] -> Proposition -> Result
verify trace formula = fromBool (fromMaybe bottom (eval trace formula))
