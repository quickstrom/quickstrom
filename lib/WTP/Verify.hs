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
import Algebra.Lattice (Lattice (..), fromBool)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import WTP.Formula.Logic
import WTP.Result
import WTP.Trace
import qualified WTP.Type as WTP
import WTP.Value

eval :: Typeable a => Formula a -> [ObservedState] -> Maybe (FValue a)
eval formula = go formula
  where
    go :: Typeable a => Formula a -> [ObservedState] -> Maybe (FValue a)
    go _ [] = Nothing
    go f steps@(current : rest) = case f of
      Literal LTrue -> pure VTrue
      Literal LFalse -> pure VFalse
      Literal (LString t) -> pure (VString t)
      Literal (LJson j) -> pure (VJson j)
      Set ps -> VSet . Set.fromList <$> traverse (flip go steps) ps
      Not p -> neg <$> go p steps
      p `And` q -> (/\) <$> go p steps <*> go q steps
      p `Or` q -> (\/) <$> go p steps <*> go q steps
      Always p
        | null steps -> pure VTrue
        | otherwise -> (/\) <$> go p steps <*> go (Always p) rest
      Equals p q -> do
        p' <- go p steps
        q' <- go q steps
        pure (fromBool (p' == q'))
      Query query -> evalQuery current query

evalQuery :: Typeable a => ObservedState -> Query a -> Maybe (FValue a)
evalQuery current = go
  where
    go :: Typeable a => Query a -> Maybe (FValue a)
    go = \case
      Get state sub ->
        go sub >>= \case
          VElement el ->
            let states = fromMaybe mempty (HashMap.lookup el (elementStates current))
             in findElementState state states
      QueryAll selector ->
        case HashMap.lookup selector (queriedElements current) of
          Just es -> pure (VSeq (map VElement es))
          _ -> pure (VSeq [])

verify :: Formula 'WTP.Bool -> [ObservedState] -> Result
verify formula trace =
  case eval formula trace of
    Just VTrue -> Accepted
    Just VFalse -> Rejected
    Nothing -> Rejected
