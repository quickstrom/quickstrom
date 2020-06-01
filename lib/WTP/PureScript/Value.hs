{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module WTP.PureScript.Value where

import Data.HashMap.Strict (HashMap)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import Data.Text.Prettyprint.Doc
import Data.Vector (Vector)
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Protolude hiding (list)
import qualified WTP.Element as Element
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap

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

instance Pretty (Value ann) where
  pretty = \case
    VNull -> "null"
    VBool b -> bool "false" "true" b
    VElementState _state -> "TODO: element state"
    VString t -> pretty (show t :: Text)
    VChar c -> pretty (show c :: Text)
    VNumber n -> pretty (show n :: Text)
    VInt n -> pretty (show n :: Text)
    VArray xs -> list (Vector.toList (Vector.map pretty xs))
    VObject obj -> encloseSep lbrace rbrace comma (map (\_ -> "") (HashMap.toList obj))
    VFunction (Function _ arg _body) -> parens ("\\" <> pretty (runIdent arg) <+> "->" <+> "<body>")

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
