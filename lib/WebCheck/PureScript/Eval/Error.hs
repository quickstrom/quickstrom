{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module WebCheck.PureScript.Eval.Error where

import Data.Generics.Sum (AsConstructor, _Ctor)
import qualified Data.Text as Text
import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn (Expr)
import Language.PureScript.Names
import Protolude
import WebCheck.PureScript.Value
import Control.Lens ((^?))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data EvalError
  = UnexpectedError (Maybe SourceSpan) Text
  | UnexpectedType (Maybe SourceSpan) Text (Value ())
  | EntryPointNotDefined (Qualified Ident)
  | NotInScope SourceSpan (Qualified Ident)
  | ForeignFunctionNotSupported SourceSpan (Qualified Ident)
  | InvalidString SourceSpan
  | InvalidBuiltInFunctionApplication SourceSpan (Expr ()) (Expr ())
  | ForeignFunctionError (Maybe SourceSpan) Text
  | Undetermined
  deriving (Show, Generic)

errorSourceSpan :: EvalError -> Maybe SourceSpan
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
    MonadError (EvalError ) m
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
    (throwError (UnexpectedError (Just ss) ("Key not present in object: " <> key)))
    pure
    (HashMap.lookup key obj)
