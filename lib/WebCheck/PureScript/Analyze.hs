{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WebCheck.PureScript.Analyze where

import Control.Lens
import Data.Generics.Product (field)
import Control.Monad.Writer (MonadWriter, tell, WriterT, execWriterT)
import qualified Data.HashMap.Strict as HashMap
import qualified Language.PureScript.CoreFn as P
import Protolude hiding (Selector)
import WebCheck.Element (Selector (..))
import WebCheck.PureScript.Eval
import WebCheck.PureScript.Value
import WebCheck.Specification (Queries)
import qualified Data.HashSet as HashSet
import qualified Language.PureScript.Names as P
import qualified Language.PureScript.AST.SourcePos as P
import Control.Monad.Fix (MonadFix)

data ExtractEnv
  = ExtractEnv
      { env :: Env' Extract
      }
  deriving (Generic)

newtype Extract a = Extract (ReaderT ExtractEnv (WriterT Queries (Except EvalError)) a)
  deriving (Functor, Applicative, Monad, MonadError EvalError, MonadReader ExtractEnv, MonadWriter Queries, MonadFix)

runExtract :: Env' Extract -> Extract () -> Either EvalError Queries
runExtract env' (Extract ma) =
  runExcept (execWriterT (runReaderT ma (ExtractEnv env')))

-- These instance methods should never be reached
instance MonadEvalQuery Extract where
  evalQuery _ _ = pure VNull
  evalNext _ = eval
  evalAlways _ = eval

extractExpr :: P.Expr EvalAnn -> Extract ()
extractExpr = \case
  Next _ e -> extractExpr e
  Always _ e -> extractExpr e
  P.App _ (BuiltIn "_queryAll" _ e1@P.Literal{}) e2@P.Literal{} -> do
      selector <- require (exprSourceSpan e1) (Proxy @"VString") =<< eval e1
      wantedStates <-
        traverse (require (exprSourceSpan e2) (Proxy @"VElementState")) . HashMap.elems
          =<< require (exprSourceSpan e2) (Proxy @"VObject")
          =<< eval e2
      tell (HashMap.singleton (Selector selector) (HashSet.fromList wantedStates))
  P.App _ (BuiltIn _ _ _) _ -> pure mempty
  P.Literal{} -> pure mempty
  P.Constructor{} -> pure mempty
  P.Accessor _ _ e -> extractExpr e
  P.ObjectUpdate _ e updates -> traverse_ (extractExpr . snd) updates >> extractExpr e
  P.Abs _ _ body -> extractExpr body
  P.App _ f e -> extractExpr f >> extractExpr e
  P.Var _ (P.Qualified (Just (P.ModuleName "Prim")) (P.Ident "undefined")) -> pass
  P.Var _ (P.Qualified (Just _mn) (P.Ident _n)) -> pass
  P.Var _ _ -> pass
  P.Case _ exprs alts -> do
    traverse_ extractExpr exprs
    for_ alts $ \(P.CaseAlternative _ result) -> 
      case result of
        Left guardedExprs -> 
          for_ guardedExprs $ \(guard', branch) ->
            extractExpr guard' >> extractExpr branch
        Right expr -> extractExpr expr
  P.Let _ bindings body -> do
    for_ bindings $ \case
      P.NonRec _ _ expr -> extractExpr expr
      P.Rec binds -> for_ binds $ \(_, expr) -> extractExpr expr
    extractExpr body

extractEntryPoint :: P.SourceSpan -> QualifiedName -> Extract ()
extractEntryPoint ss entryPoint = do
  env' <- view (field @"env")
  case envLookupTopLevel entryPoint env' of
    Just expr -> extractExpr expr
    Nothing -> throwError (NotInScope ss (Left entryPoint))