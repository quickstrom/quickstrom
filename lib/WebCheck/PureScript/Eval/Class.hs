{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module WebCheck.PureScript.Eval.Class where

import Language.PureScript.CoreFn (Expr)
import Protolude
import WebCheck.PureScript.Eval.Error (EvalError)
import WebCheck.PureScript.Value (Function(..), Value)
import WebCheck.PureScript.Eval.Env
import WebCheck.PureScript.Eval.Ann
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript as P
import Control.Monad.Fix (MonadFix)

class (MonadError EvalError m, MonadFix m) => MonadEvalQuery m where
  evalQuery :: Expr EvalAnn -> Expr EvalAnn -> m (Value EvalAnn)
  evalNext ::  Expr EvalAnn -> m (Value EvalAnn)
  evalAlways ::  Expr EvalAnn -> m (Value EvalAnn)