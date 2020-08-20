{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Quickstrom.PureScript.Eval.Class where

import Control.Monad.Fix (MonadFix)
import Language.PureScript.CoreFn (Expr)
import Quickstrom.Prelude
import Quickstrom.PureScript.Eval.Ann
import Quickstrom.PureScript.Eval.Error (EvalError)
import Quickstrom.PureScript.Value (Value)

class (MonadError EvalError m, MonadFix m) => MonadEvalQuery m where
  evalQuery :: Expr EvalAnn -> Expr EvalAnn -> m (Value EvalAnn)
  evalNext :: EvalAnn -> Expr EvalAnn -> m (Value EvalAnn)
  evalAlways :: EvalAnn -> Expr EvalAnn -> m (Value EvalAnn)
