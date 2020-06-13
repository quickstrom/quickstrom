{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module WebCheck.PureScript.Eval.Class where

import Language.PureScript.CoreFn (Expr)
import Protolude
import WebCheck.PureScript.Eval.Error (EvalError)
import WebCheck.PureScript.Value (Function, Value)

class MonadError EvalError m => MonadEval m where

  type Ann m :: *

  eval :: Expr (Ann m) -> m (Value (Ann m))

  evalFunc :: Function (Ann m) -> (Value (Ann m)) -> m (Value (Ann m))
