{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module WebCheck.PureScript.Pretty where

import Data.Text.Prettyprint.Doc
import Language.PureScript.AST (SourceSpan, sourcePosColumn, sourcePosLine, spanEnd, spanName, spanStart)
import Protolude
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Eval.Name

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

prettyName :: Name -> Doc ann
prettyName (Name n) = pretty n

prettyModuleName :: ModuleName -> Doc ann
prettyModuleName (ModuleName n) = pretty n

prettyQualifiedName :: QualifiedName -> Doc ann
prettyQualifiedName (QualifiedName mns n) =
  concatWith (surround dot) (prettyModuleName <$> mns) <> dot <> prettyName n

prettyEvalError :: EvalError -> Doc ann
prettyEvalError = \case
  UnexpectedError _ t -> "Unexpected error:" <+> pretty t
  UnexpectedType _ t val -> "Expected value of type" <+> pretty t <+> "but got" <+> pretty val
  EntryPointNotDefined qn -> "Entry point not in scope:" <+> prettyQualifiedName qn
  InvalidEntryPoint n -> "Entry point is invalid:" <+> prettyName n
  NotInScope _ qn -> "Not in scope:" <+> either prettyQualifiedName prettyName qn
  ForeignFunctionNotSupported _ qn -> "Foreign function is not supported in WebCheck:" <+> prettyQualifiedName qn
  InvalidString _ -> "Invalid string"
  InvalidBuiltInFunctionApplication _ _fn _param -> "Invalid function application"
  ForeignFunctionError _ t -> pretty t
  InvalidURI _ input t -> "Invalid URI:" <> colon <+> pretty input <> comma <+> pretty t
  UnsupportedQuery _ -> "Unsupported query"
  UnsupportedQueryDependency _ _ -> "Queries cannot be constructed from the results of other queries"
  Undetermined -> "The formula cannot be determined as there are not enough observed states"

prettyEvalErrorWithSourceSpan :: EvalError -> Doc ann
prettyEvalErrorWithSourceSpan err =
  let prefix = case errorSourceSpan err of
        Just ss -> prettySourceSpan ss <> ":" <> line <> "error:"
        Nothing -> "<no source information>" <> "error:"
   in prefix <+> prettyEvalError err
