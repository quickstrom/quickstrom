{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module WebCheck.PureScript.Eval.Ann where

import Protolude
import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CF

data ApplyForeign = ApplyForeign (P.Qualified P.Ident) [P.Ident]
  deriving (Show, Generic)

data EvalAnn = EvalAnn {annSourceSpan :: P.SourceSpan, annMeta :: Maybe CF.Meta, annApplyForeign :: Maybe ApplyForeign}
  deriving (Show, Generic)

evalAnnFromAnn :: CF.Ann -> EvalAnn
evalAnnFromAnn (ss, _, _, meta) = EvalAnn ss meta Nothing