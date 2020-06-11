{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module WebCheck.PureScript.EvalError where

import Protolude
import Language.PureScript.Names
import WebCheck.PureScript.Value
import Language.PureScript.CoreFn (Expr)
import Language.PureScript.AST (SourceSpan)

data EvalError ann
  = UnexpectedError (Maybe SourceSpan) Text
  | UnexpectedType (Maybe SourceSpan) Text (Value ann)
  | EntryPointNotDefined (Qualified Ident)
  | NotInScope SourceSpan (Qualified Ident)
  | ForeignFunctionNotSupported SourceSpan (Qualified Ident)
  | InvalidString SourceSpan
  | InvalidBuiltInFunctionApplication SourceSpan (Expr ann) (Expr ann)
  | ForeignFunctionError (Maybe SourceSpan) Text
  | Undetermined
  deriving (Show, Generic)

errorSourceSpan :: EvalError ann -> Maybe SourceSpan
errorSourceSpan = \case
  UnexpectedError ss _ -> ss
  UnexpectedType ss _ _ -> ss
  EntryPointNotDefined _ -> Nothing
  NotInScope ss _ -> Just ss
  ForeignFunctionNotSupported ss _ -> Just ss
  InvalidString ss -> Just ss
  InvalidBuiltInFunctionApplication ss _ _ -> Just ss
  ForeignFunctionError ss _ -> ss
  Undetermined -> Nothing
