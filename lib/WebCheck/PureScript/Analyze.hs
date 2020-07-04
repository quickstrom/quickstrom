{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WebCheck.PureScript.Analyze where

import Control.Monad.Writer (tell, WriterT, execWriterT)
import qualified Data.HashMap.Strict as HashMap
import qualified Language.PureScript.CoreFn as P
import Protolude hiding (Selector)
import WebCheck.Element (Selector (..))
import WebCheck.PureScript.Eval.Class
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Eval.Interpret
import WebCheck.PureScript.Value
import WebCheck.Specification (Queries)
import qualified Data.HashSet as HashSet
import qualified Language.PureScript.Names as P

data ExtractError

extract :: P.Expr P.Ann -> Either ExtractError Queries
extract = runExcept . execWriterT . go
  where
    go :: P.Expr P.Ann -> WriterT Queries (Except ExtractError) ()
    go = \case
        Next _ e -> go e
        Always _ e -> go e
        P.App _ (BuiltIn "_queryAll" _ e1) e2 -> do
            -- selector <- require (exprSourceSpan e1) (Proxy @"VString") =<< eval e1
            -- wantedStates <-
            --   traverse (require (exprSourceSpan e2) (Proxy @"VElementState")) . HashMap.elems
            --     =<< require (exprSourceSpan e2) (Proxy @"VObject")
            --     =<< eval e2
            tell (HashMap.singleton (Selector _selector) (HashSet.fromList _wantedStates))
        P.App _ (BuiltIn _ _ _) _ -> pure mempty
        P.Literal{} -> pure mempty
        P.Constructor{} -> pure mempty
        P.Accessor _ _ e -> go e
        P.ObjectUpdate _ e updates -> traverse_ (go . snd) updates >> go e
        P.Abs _ _ body -> go body
        P.App _ f e -> go f >> go e
        P.Var _ (P.Qualified (Just (P.ModuleName "Prim")) (P.Ident "undefined")) -> pass
        P.Var _ (P.Qualified (Just mn) (P.Ident n)) -> pass
        P.Case _ exprs alts -> _
        P.Let _ bindings body -> _
        
