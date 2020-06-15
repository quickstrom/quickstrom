{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module WebCheck.PureScript.Value where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Prettyprint.Doc
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Protolude hiding (list)
import qualified WebCheck.Element as Element

data Value ann
  = VNull
  | VBool Bool
  | VElementState Element.ElementState
  | VString Text
  | VChar Char
  | VNumber Double
  | VInt Int
  | VArray (Vector (Value ann))
  | VObject (HashMap Text (Value ann))
  | VFunction (Function ann)
  | VDefer (Defer ann)
  deriving (Show, Generic, Functor)

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
    VObject obj -> encloseSep lbrace rbrace (comma <> space) (map (\(k, v) -> pretty k <> ":" <+> pretty v) (HashMap.toList obj))
    VFunction f -> pretty f
    VDefer f -> pretty f

type ClosureEnv ann = Map (Qualified Ident) (Either (Expr ann) (Value ann))

data Function ann = Function (ClosureEnv ann) Ident (Expr ann)
  deriving (Show, Generic)

instance Functor Function where
  fmap f (Function env ident expr) = Function (fmap (bimap (fmap f) (fmap f)) env) ident (fmap f expr)

instance Pretty (Function ann) where
  pretty (Function _ arg _) = parens (backslash <> pretty (runIdent arg) <+> "->" <+> "<body>")

data Defer ann = Defer (ClosureEnv ann) (Expr ann)
  deriving (Show, Generic)

instance Functor Defer where
  fmap f (Defer env expr) = Defer (fmap (bimap (fmap f) (fmap f)) env) (fmap f expr)

instance Pretty (Defer ann) where
  pretty (Defer _ _) = "<deferred>"
