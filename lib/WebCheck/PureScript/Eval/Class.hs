{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module WebCheck.PureScript.Eval.Class where

import Language.PureScript.CoreFn (Expr)
import WebCheck.Prelude
import WebCheck.PureScript.Eval.Error (EvalError)
import WebCheck.PureScript.Value (Value)
import WebCheck.PureScript.Eval.Ann
import Control.Monad.Fix (MonadFix)

class (MonadError EvalError m, MonadFix m) => MonadEvalQuery m where
  evalQuery :: Expr EvalAnn -> Expr EvalAnn -> m (Value EvalAnn)
  evalNext ::  EvalAnn -> Expr EvalAnn -> m (Value EvalAnn)
  evalAlways ::  EvalAnn -> Expr EvalAnn -> m (Value EvalAnn)