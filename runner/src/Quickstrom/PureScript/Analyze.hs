{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Quickstrom.PureScript.Analyze where

import Control.Lens
import Control.Monad.Fix (MonadFix)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Language.PureScript.AST.SourcePos as P
import qualified Language.PureScript.CoreFn as P
import qualified Language.PureScript.Names as P
import Quickstrom.Element (Selector (..))
import Quickstrom.Prelude
import Quickstrom.PureScript.Eval
import Quickstrom.Specification (Queries)

data SimpleEvalEnv
  = SimpleEvalEnv
      { env :: Env' SimpleEval
      }
  deriving (Generic)

newtype SimpleEval a = SimpleEval (ReaderT SimpleEvalEnv (Except EvalError) a)
  deriving (Functor, Applicative, Monad, MonadError EvalError, MonadReader SimpleEvalEnv, MonadFix)

runSimpleEval :: Env' SimpleEval -> SimpleEval a -> Either EvalError a
runSimpleEval env' (SimpleEval ma) =
  runExcept (runReaderT ma (SimpleEvalEnv env'))

-- These instance methods should never be reached
instance MonadEvalQuery SimpleEval where
  evalQuery expr _ = throwError (InvalidQueryDependency (exprSourceSpan expr))

  evalNext _ = eval

  evalAlways _ = eval

  evalUntil _ _ = eval

type Visited = HashSet QualifiedName

type Extract = ReaderT (Env' SimpleEval) (WriterT Queries (StateT Visited (Except EvalError)))

extractExpr :: P.Expr EvalAnn -> Extract ()
extractExpr = \case
  Next _ e -> extractExpr e
  Always _ e -> extractExpr e
  Until _ p q -> extractExpr p >> extractExpr q
  -- We need to ignore all the type class dictionaries that are passed in.
  P.App _ (P.App _ (P.App _ (P.App _ (BuiltIn f _ _) _) _) e1) e2 | f `elem` ["queryAll", "queryOne"] ->
    case (e1, e2) of
      (P.Var ann' _, _) -> throwError (UnsupportedQueryExpression (annSourceSpan ann'))
      (_, P.Var ann' _) -> throwError (UnsupportedQueryExpression (annSourceSpan ann'))
      _ -> do
        env' <- ask
        let result = runSimpleEval env' $ do
              selector <- require (exprSourceSpan e1) (Proxy @"VString") =<< eval e1
              wantedStates <-
                traverse (require (exprSourceSpan e2) (Proxy @"VElementState")) . HashMap.elems
                  =<< require (exprSourceSpan e2) (Proxy @"VObject")
                  =<< eval e2
              pure (HashMap.singleton (Selector selector) (HashSet.fromList wantedStates))
        either throwError tell result
  P.Literal _ lit -> case lit of
    P.ArrayLiteral xs -> traverse_ extractExpr xs
    P.ObjectLiteral pairs -> do
      traverse_ (extractExpr . snd) pairs
    _ -> pass
  P.Constructor {} -> pass
  P.Accessor _ _ e -> extractExpr e
  P.ObjectUpdate _ e updates -> traverse_ (extractExpr . snd) updates >> extractExpr e
  P.Abs _ _ body -> extractExpr body
  P.App _ f e -> extractExpr f >> extractExpr e
  P.Var _ (P.Qualified (Just (P.ModuleName "Prim")) (P.Ident "undefined")) -> pass
  P.Var EvalAnn {annApplyForeign = Just {}} _qi -> pass
  P.Var ann qi ->
    case fromQualifiedIdent qi of
      Left qn -> do
        visited <- get
        unless (HashSet.member qn visited) $ do
          modify (HashSet.insert qn)
          env' <- ask
          case envLookupTopLevel qn env' of
            Just expr -> do
              extractExpr expr
            Nothing -> throwError (NotInScope (annSourceSpan ann) (Left qn))
      Right _ -> pass
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
  env' <- ask
  case envLookupTopLevel entryPoint env' of
    Just expr -> extractExpr expr
    Nothing -> throwError (NotInScope ss (Left entryPoint))

runExtract :: Env' SimpleEval -> Extract () -> Either EvalError Queries
runExtract env' ma =
  runExcept (evalStateT (execWriterT (runReaderT ma env')) mempty)
