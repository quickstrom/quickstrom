{-# LANGUAGE DeriveGeneric #-}

module Quickstrom.PureScript.Eval.Ann where

import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CF
import Quickstrom.Prelude
import Quickstrom.PureScript.Eval.Name

data ApplyForeign = ApplyForeign QualifiedName [Name]
  deriving (Show, Generic)

data EvalAnn = EvalAnn {annSourceSpan :: P.SourceSpan, annMeta :: Maybe CF.Meta, annApplyForeign :: Maybe ApplyForeign}
  deriving (Show, Generic)

evalAnnFromAnn :: CF.Ann -> EvalAnn
evalAnnFromAnn (ss, _, _, meta) = EvalAnn ss meta Nothing
