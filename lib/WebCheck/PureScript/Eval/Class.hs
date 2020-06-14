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
import WebCheck.PureScript.Value (Function(..), Value, Env, envBindValue)
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript as P
