{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module WebCheck.PureScript.Eval.Class where

import Language.PureScript.CoreFn (Expr)
import Protolude
import WebCheck.PureScript.Eval.Error (EvalError)
import WebCheck.PureScript.Value (Function, Value)
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript as P

class MonadError EvalError m => MonadEval m where

  type Ann m :: *

  eval :: Expr (Ann m) -> m (Value (Ann m))

  evalFunc :: Function (Ann m) -> (Value (Ann m)) -> m (Value (Ann m))

pattern BuiltIn :: Text -> a -> Expr a -> Expr a
pattern BuiltIn name ann p <- CF.App ann (CF.Var _ (P.Qualified (Just (P.ModuleName "WebCheck.DSL")) (P.Ident name))) p

pattern Always :: a -> Expr a -> Expr a
pattern Always ann p <- BuiltIn "always" ann p

pattern Next :: a -> Expr a -> Expr a
pattern Next ann p <- BuiltIn "next" ann p
