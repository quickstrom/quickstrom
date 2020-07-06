{-# LANGUAGE DeriveGeneric #-}

module WebCheck.PureScript.Eval.Ann where

import WebCheck.Prelude
import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CF
import WebCheck.PureScript.Eval.Name

data ApplyForeign = ApplyForeign QualifiedName [Name]
  deriving (Show, Generic)

data EvalAnn = EvalAnn {annSourceSpan :: P.SourceSpan, annMeta :: Maybe CF.Meta, annApplyForeign :: Maybe ApplyForeign}
  deriving (Show, Generic)

evalAnnFromAnn :: CF.Ann -> EvalAnn
evalAnnFromAnn (ss, _, _, meta) = EvalAnn ss meta Nothing