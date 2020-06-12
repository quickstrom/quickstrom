{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module WebCheck.PureScript.Eval.Class where

import Protolude
import Language.PureScript.CoreFn (Expr)
import WebCheck.PureScript.Value
import WebCheck.PureScript.Eval.Error

class MonadError EvalError m => MonadEval m where
    type Ann m :: *
    eval :: Expr (Ann m) -> m (Value (Ann m))
    evalFunc :: Function (Ann m) -> (Value (Ann m)) -> m (Value (Ann m))