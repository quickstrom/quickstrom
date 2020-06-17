{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WebCheck.PureScript.Pretty where

import Data.Text.Prettyprint.Doc
import Language.PureScript.AST (SourceSpan, sourcePosColumn, sourcePosLine, spanEnd, spanName, spanStart)
import Language.PureScript.Names
import Protolude
import WebCheck.PureScript.Eval.Error

prettySourceSpan :: SourceSpan -> Doc ann
prettySourceSpan ss =
  pretty (spanName ss)
    <> colon
    <> pretty (sourcePosLine (spanStart ss))
    <> colon
    <> pretty (sourcePosColumn (spanStart ss))
    <> "-"
    <> pretty (sourcePosLine (spanEnd ss))
    <> colon
    <> pretty (sourcePosColumn (spanEnd ss))

prettyQualifiedIdent :: Qualified Ident -> Doc ann
prettyQualifiedIdent qn = pretty (showQualified runIdent qn)

prettyEvalError :: EvalError -> Doc ann
prettyEvalError = \case
  UnexpectedError _ t -> "Unexpected error:" <+> pretty t
  UnexpectedType _ t val -> "Expected value of type" <+> pretty t <+> "but got" <+> pretty val
  EntryPointNotDefined qn -> "Entry point not in scope:" <+> prettyQualifiedIdent qn
  NotInScope _ qn -> "Not in scope:" <+> prettyQualifiedIdent qn
  ForeignFunctionNotSupported _ mn ident -> "Foreign function is not supported in WebCheck:" <+> prettyQualifiedIdent (Qualified (Just mn) ident)
  InvalidString _ -> "Invalid string"
  InvalidBuiltInFunctionApplication _ _fn _param -> "Invalid function application"
  ForeignFunctionError _ t -> pretty t
  Undetermined -> "The formula cannot be determined as there are not enough observed states"

prettyEvalErrorWithSourceSpan :: EvalError -> Doc ann
prettyEvalErrorWithSourceSpan err =
  let prefix = case errorSourceSpan err of
        Just ss -> prettySourceSpan ss <> ":" <> line <> "error:"
        Nothing -> "<no source information>" <> "error:"
   in prefix <+> prettyEvalError err
