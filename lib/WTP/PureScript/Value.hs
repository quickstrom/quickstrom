{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WTP.PureScript.Value where

import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Data.Vector (Vector)
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Protolude
import qualified WTP.Element as Element
import qualified Data.Map as Map

data Value ann
  = VNull
  | VBool Bool
  | VElementState Element.ElementState
  | VString Text
  | VChar Char
  | VNumber Scientific
  | VInt Integer
  | VArray (Vector (Value ann))
  | VObject (HashMap Text (Value ann))
  | VFunction (Function ann)
  deriving (Show, Generic)

data Function ann = Function (Env ann) Ident (Expr ann)
  deriving (Show, Generic)

newtype Env ann = Env (Map (Qualified Ident) (Either (Expr ann) (Value ann)))
  deriving (Show, Semigroup, Monoid)

envBindValue :: Qualified Ident -> Value ann -> Env ann
envBindValue qn expr = Env (Map.singleton qn (Right expr))

envBindExpr :: Qualified Ident -> Expr ann -> Env ann
envBindExpr qn expr = Env (Map.singleton qn (Left expr))

withoutLocals :: Env ann -> Env ann
withoutLocals (Env ms) = Env (Map.filterWithKey (\(Qualified modules _) _ -> isJust modules) ms)

envLookup :: Qualified Ident -> Env ann -> Maybe (Either (Expr ann) (Value ann))
envLookup qn (Env env) = Map.lookup qn env