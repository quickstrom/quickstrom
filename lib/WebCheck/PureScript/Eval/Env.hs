{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WebCheck.PureScript.Eval.Env where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Protolude hiding (list)
import WebCheck.PureScript.Eval.Name

data Env expr value ff ann
  = Env
      { envTopLevels :: HashMap QualifiedName (expr ann)
      , envLocals :: HashMap Name (value ann)
      , envForeignFunctions :: HashMap QualifiedName (ff ann)
      }
      deriving (Generic, Show)

instance Semigroup (Env expr value ff ann) where
    Env b1 l1 f1 <> Env b2 l2 f2 = Env (b1 <> b2) (l1 <> l2) (f1 <> f2)

instance Monoid (Env expr value ff ann) where
    mempty = Env mempty mempty mempty

instance (Functor expr, Functor value, Functor ff) => Functor (Env expr value ff) where
  fmap f (Env b l ffs) = Env (map (map f) b) (map (map f) l) (map (map f) ffs)

envBindLocal :: Name -> value ann -> Env expr value ff ann
envBindLocal qn expr = Env mempty (HashMap.singleton qn expr) mempty

envBindTopLevel :: QualifiedName -> expr ann -> Env expr value ff ann
envBindTopLevel qn expr = Env (HashMap.singleton qn expr) mempty mempty

withoutLocals :: e ~ Env expr value ff ann => e -> e
withoutLocals env = env { envLocals = mempty }

envLookupTopLevel :: QualifiedName -> Env expr value ff ann -> Maybe (expr ann)
envLookupTopLevel qn env =
  HashMap.lookup qn (envTopLevels env)

envLookupLocal :: Name -> Env expr value ff ann -> Maybe (value ann)
envLookupLocal n env =
  HashMap.lookup n (envLocals env)
