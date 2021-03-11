{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Quickstrom.PureScript.Eval.Error where

import Control.Lens ((^?))
import Data.Generics.Sum (AsConstructor, _Ctor)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn (Expr)
import Quickstrom.Prelude
import Quickstrom.PureScript.Eval.Name
import Quickstrom.PureScript.Value

data EvalError
  = UnexpectedError (Maybe SourceSpan) Text
  | UnexpectedType (Maybe SourceSpan) Text (Value ())
  | EntryPointNotDefined QualifiedName
  | InvalidEntryPoint Name
  | NotInScope SourceSpan (Either QualifiedName Name)
  | ForeignFunctionNotSupported SourceSpan QualifiedName
  | InvalidString SourceSpan
  | InvalidBuiltInFunctionApplication SourceSpan (Expr ()) (Expr ())
  | InvalidBuiltInReference SourceSpan Text
  | ForeignFunctionError (Maybe SourceSpan) Text
  | InvalidURI (Maybe SourceSpan) Text Text
  | UnsupportedQueryExpression SourceSpan
  | InvalidQueryDependency SourceSpan
  | Undetermined
  deriving (Show, Generic)

errorSourceSpan :: EvalError -> Maybe SourceSpan
errorSourceSpan = \case
  UnexpectedError ss _ -> ss
  UnexpectedType ss _ _ -> ss
  EntryPointNotDefined _ -> Nothing
  InvalidEntryPoint _ -> Nothing
  NotInScope ss _ -> Just ss
  ForeignFunctionNotSupported ss _ -> Just ss
  InvalidString ss -> Just ss
  InvalidBuiltInFunctionApplication ss _ _ -> Just ss
  InvalidBuiltInReference ss _ -> Just ss
  ForeignFunctionError ss _ -> ss
  InvalidURI ss _ _ -> ss
  UnsupportedQueryExpression ss -> Just ss
  InvalidQueryDependency ss -> Just ss
  Undetermined -> Nothing

unexpectedType :: (MonadError EvalError m) => SourceSpan -> Text -> Value ann -> m a
unexpectedType ss typ v =
  throwError
    ( UnexpectedType
        (Just ss)
        typ
        (void v)
    )

require ::
  forall (ctor :: Symbol) s t a b ann m.
  ( KnownSymbol ctor,
    AsConstructor ctor s t a b,
    s ~ Value ann,
    t ~ Value ann,
    a ~ b,
    MonadError (EvalError) m
  ) =>
  SourceSpan ->
  Proxy ctor ->
  Value ann ->
  m b
require ss (ctor :: Proxy ctor) v = case v ^? _Ctor @ctor of
  Just x -> pure x
  Nothing -> unexpectedType ss (Text.drop 1 (Text.pack (symbolVal ctor))) v

accessField :: MonadError EvalError m => SourceSpan -> Text -> HashMap Text (Value ann) -> m (Value ann)
accessField ss key obj =
  maybe
    (throwError (UnexpectedError (Just ss) ("Key '" <> key <> "' not present in object: " <> show (void obj))))
    pure
    (HashMap.lookup key obj)
