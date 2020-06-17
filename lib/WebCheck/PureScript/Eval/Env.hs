{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WebCheck.PureScript.Eval.Env where

import qualified Data.Map as Map
import Language.PureScript.Names
import Protolude hiding (list)

data Env expr value ff ann
  = Env
      { envTopLevels :: Map (ModuleName, Ident) (expr ann)
      , envLocals :: Map Ident (value ann)
      , envForeignFunctions :: Map (ModuleName, Ident) (ff ann)
      }
      deriving (Generic, Show)

instance Semigroup (Env expr value ff ann) where
    Env b1 l1 f1 <> Env b2 l2 f2 = Env (b1 <> b2) (l1 <> l2) (f1 <> f2)

instance Monoid (Env expr value ff ann) where
    mempty = Env mempty mempty mempty

instance (Functor expr, Functor value, Functor ff) => Functor (Env expr value ff) where
  fmap f (Env b l ffs) = Env (Map.map (fmap f) b) (Map.map (fmap f) l) (Map.map (fmap f) ffs)

envBindLocal :: Ident -> value ann -> Env expr value ff ann
envBindLocal qn expr = Env mempty (Map.singleton qn expr) mempty

envBindTopLevel :: ModuleName -> Ident -> expr ann -> Env expr value ff ann
envBindTopLevel mn ident expr = Env (Map.singleton (mn, ident) expr) mempty mempty

withoutLocals :: e ~ Env expr value ff ann => e -> e
withoutLocals env = env { envLocals = mempty }

envLookup :: Qualified Ident -> Env expr value ff ann -> Maybe (Either (expr ann) (value ann))
envLookup (Qualified (Just mn) ident) env =
  Left <$> Map.lookup (mn, ident) (envTopLevels env)
envLookup (Qualified Nothing ident) env =
  Right <$> Map.lookup ident (envLocals env)
