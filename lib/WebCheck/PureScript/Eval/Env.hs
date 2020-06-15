{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WebCheck.PureScript.Eval.Env where

import qualified Data.Map as Map
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Protolude hiding (list)
import WebCheck.PureScript.Value
import WebCheck.PureScript.Eval.Ann
import Language.PureScript (SourceSpan)

data Env m ann
  = Env
      { envBindings :: Map (Qualified Ident) (Either (Expr ann) (Value ann))
      , envForeignFunctions :: Map (Qualified Ident) (EvalForeignFunction m EvalAnn)
      }
      deriving (Generic)

instance Semigroup (Env m ann) where
    Env b1 f1 <> Env b2 f2 = Env (b1 <> b2) (f1 <> f2)

instance Monoid (Env m ann) where
    mempty = Env mempty mempty


type EvalForeignFunction m ann = SourceSpan -> [Value ann] -> m (Value ann)

instance Functor (Env m) where
  fmap f (Env b ffs) = Env (Map.map (bimap (fmap f) (fmap f)) b) ffs

envBindValue :: Qualified Ident -> Value ann -> Env m ann
envBindValue qn expr = Env (Map.singleton qn (Right expr)) mempty

envBindExpr :: Qualified Ident -> Expr ann -> Env m ann
envBindExpr qn expr = Env (Map.singleton qn (Left expr)) mempty

withoutLocals :: Env m ann -> Env m ann
withoutLocals (Env ms ffs) = Env (Map.filterWithKey (\(Qualified modules _) _ -> isJust modules) ms) ffs

envLookup :: Qualified Ident -> Env m ann -> Maybe (Either (Expr ann) (Value ann))
envLookup qn = Map.lookup qn . envBindings
