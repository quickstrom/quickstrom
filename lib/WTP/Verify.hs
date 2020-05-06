{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module WTP.Verify
  ( eval,
    verify,
  )
where

import Algebra.Heyting (Heyting (..))
import Algebra.Lattice (Lattice (..), bottom, fromBool, top)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Maybe (fromMaybe)
import WTP.Formula.Logic
import WTP.Result
import WTP.Trace
import Control.Monad.Freer (sendM, reinterpret, Eff, runM, type (~>))

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
  p `And` q -> (/\) <$> eval steps p <*> eval steps q
  p `Or` q -> (\/) <$> eval steps p <*> eval steps q
  Always p
    | null steps -> pure True
    | otherwise -> (/\) <$> eval steps p <*> eval rest (Always p)
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
verify trace formula =
  case eval trace formula of
    Just True -> Accepted
    Just False -> Rejected
    Nothing -> Rejected
