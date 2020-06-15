{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module WebCheck.PureScript.Eval where

import Control.Lens hiding (op)
import Control.Monad.Fix (MonadFix)
import Data.Generics.Product (HasField, field)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Vector as Vector
import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn hiding (Ann)
import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString, mkString)
import Protolude hiding (HasField, Meta, moduleName)
import qualified WebCheck.Element as WebCheck
import WebCheck.PureScript.Eval.Ann
import WebCheck.PureScript.Eval.Class
import WebCheck.PureScript.Eval.Env
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Value

pattern BuiltIn :: Text -> a -> Expr a -> Expr a
pattern BuiltIn name ann p <- CF.App ann (CF.Var _ (Qualified (Just (ModuleName "WebCheck.DSL")) (Ident name))) p

pattern Always :: a -> Expr a -> Expr a
pattern Always ann p <- BuiltIn "always" ann p

pattern Next :: a -> Expr a -> Expr a
pattern Next ann p <- BuiltIn "next" ann p

type Eval r m =
  ( MonadFix m,
    MonadReader r m,
    HasField "env" r r (Env m EvalAnn) (Env m EvalAnn),
    MonadEvalQuery m
  )

-- data EvalEnv m
--   = EvalEnv
--       { env :: Env EvalAnn,
--         observedStates :: [(Int, WebCheck.ObservedState)],
--         foreignFunctions :: Map (Qualified Ident) (EvalForeignFunction m)
--       }
--   deriving (Generic)
--
-- newtype Eval r m a = Eval (ExceptT EvalError (ReaderT (EvalEnv (Eval r m)) m) a)
--   deriving (Functor, Applicative, Monad, MonadError EvalError, MonadFix, MonadReader (EvalEnv (Eval r m)))
--
-- runEval ::
--   Env EvalAnn ->
--   [WebCheck.ObservedState] ->
--   Map (Qualified Ident) (EvalForeignFunction (Eval r m)) ->
--   Eval r m a ->
--   m (Either EvalError a)
-- runEval env observedStates ffs (Eval r ma) = runReaderT (runExceptT ma) (EvalEnv env (zip [1 ..] observedStates) ffs)

-- instance (MonadQueries m) => MonadQueries (Eval r m) where
--   evalQuery p q s = Eval (lift (lift (evalQuery p q s)))

{-# SCC eval "eval" #-}
eval :: Eval r m => Expr EvalAnn -> m (Value EvalAnn)
eval expr = do
  env <- view (field @"env") <$> ask
  case expr of
    Next ann p -> evalNext ann p
    Always ann p -> evalAlways ann p
    App _ (BuiltIn "_queryAll" _ p) q -> evalQuery p q
    App _ (BuiltIn "trace" _ label) p -> do
      _t <- require (exprSourceSpan label) (Proxy @"VString") =<< eval label
      -- traceM
      --   ( prettyText
      --       ( prettySourceSpan (exprSourceSpan expr) <> colon
      --           <+> "trace: in state"
      --           <+> pretty n <> colon
      --           <+> pretty t
      --       )
      --   )
      eval p
    BuiltIn "_property" _ p -> do
      name <- require (exprSourceSpan p) (Proxy @"VString") =<< eval p
      pure (VElementState (WebCheck.Property name))
    BuiltIn "_attribute" _ p -> do
      name <- require (exprSourceSpan p) (Proxy @"VString") =<< eval p
      pure (VElementState (WebCheck.Attribute name))
    BuiltIn "cssValue" _ p -> do
      name <- require (exprSourceSpan p) (Proxy @"VString") =<< eval p
      pure (VElementState (WebCheck.CssValue name))
    -- General cases
    Literal (EvalAnn ss _ _) lit -> case lit of
      NumericLiteral n' -> pure (either (VInt . fromInteger) (VNumber . realToFrac) n')
      StringLiteral s -> VString <$> evalString ss s
      CharLiteral c -> pure (VChar c)
      BooleanLiteral b -> pure (VBool b)
      ArrayLiteral xs -> VArray . Vector.fromList <$> traverse eval xs
      ObjectLiteral pairs -> do
        pairs' <- for pairs $ \(field', value) ->
          (,) <$> evalString ss field' <*> eval value
        pure (VObject (HashMap.fromList pairs'))
    Constructor (EvalAnn ss (Just IsNewtype) _) _ _ _fieldNames -> do
      pure (VFunction (Function mempty (Ident "value") (Var (EvalAnn ss Nothing Nothing) (unqualifiedName "value"))))
    Constructor ann _typeName ctorName fieldNames -> do
      let body =
            Literal
              ann
              ( ObjectLiteral
                  [ (mkString "constructor", Literal ann (StringLiteral (mkString (runProperName ctorName)))),
                    (mkString "fields", Literal ann (ArrayLiteral (map (Var ann . Qualified Nothing) fieldNames)))
                  ]
              )
      eval (foldr (Abs ann) body fieldNames)
    Accessor (EvalAnn ss _ _) prop objExpr -> do
      key <- evalString ss prop
      obj <- require ss (Proxy @"VObject") =<< eval objExpr
      accessField ss key obj
    ObjectUpdate (EvalAnn ss _ _) objExpr updates -> do
      obj <- require ss (Proxy @"VObject") =<< eval objExpr
      updates' <- for updates $ \(field', expr') ->
        (,) <$> evalString ss field' <*> eval expr'
      pure (VObject (obj <> HashMap.fromList updates'))
    Abs _ann arg body -> pure (VFunction (Function (envBindings env) arg body))
    App _ func param -> do
      func' <- require (exprSourceSpan func) (Proxy @"VFunction") =<< eval func
      param' <- eval param
      evalFunc func' param'
    Var _ (Qualified (Just (ModuleName "Prim")) (Ident "undefined")) -> pure (VObject mempty)
    Var (EvalAnn ss _ (Just (ApplyForeign qn names))) _ -> do
      params <- for names $ \n -> envLookupEval ss (Qualified Nothing n)
      case Map.lookup qn (envForeignFunctions env) of
        Just f -> f ss params
        _ -> throwError (ForeignFunctionNotSupported ss qn)
    Var (EvalAnn ss _ Nothing) qn -> envLookupEval ss qn
    Case (EvalAnn ss _ _) exprs alts -> do
      values <- traverse eval exprs
      evalCaseAlts ss values alts
    Let (EvalAnn _ss _ _) bindings body -> do
      let bindingEnv env'' = \case
            NonRec _ name expr' -> do
              pure (env'' <> envBindValue (Qualified Nothing name) (VDefer (Defer (envBindings env'') expr')))
            Rec binds -> do
              rec recEnv <-
                    fold
                      <$> traverse
                        ( \((_, name), expr') ->
                            envBindValue (Qualified Nothing name) <$> pure (VDefer (Defer (envBindings (env'' <> recEnv)) expr'))
                        )
                        binds
              pure recEnv
      newEnv <- foldM bindingEnv env bindings
      local (field @"env" %~ (<> newEnv)) (eval body)

evalString :: Eval r m => SourceSpan -> PSString -> m Text
evalString ss s =
  case decodeString s of
    Just t -> pure t
    Nothing -> throwError (InvalidString ss)

evalStringExpr :: Eval r m => Expr EvalAnn -> m Text
evalStringExpr (Literal ann (StringLiteral s)) =
  case decodeString s of
    Just t -> pure t
    Nothing -> throwError (InvalidString (annSourceSpan ann))
evalStringExpr expr = throwError (InvalidString (annSourceSpan (extractAnn expr)))

envLookupEval :: (Eval r m, MonadFix m) => SourceSpan -> Qualified Ident -> m (Value EvalAnn)
envLookupEval ss qn = do
  env' <- view (field @"env")
  let onValue (VDefer (Defer env'' expr')) = local (field @"env" .~ Env env'' (envForeignFunctions env')) (eval expr')
      onValue val = pure val
  case envLookup qn env' of
    Just r -> either (local (field @"env" %~ withoutLocals) . eval) onValue r
    Nothing -> throwError (NotInScope ss qn)

qualifiedName :: Text -> Text -> Qualified Ident
qualifiedName mn localName = Qualified (Just (ModuleName mn)) (Ident localName)

unqualifiedName :: Text -> Qualified Ident
unqualifiedName localName = Qualified Nothing (Ident localName)

asQualifiedVar :: Expr EvalAnn -> Maybe (Text, Text)
asQualifiedVar (Var _ qn) = asQualifiedName qn
asQualifiedVar _ = Nothing

asQualifiedName :: Qualified Ident -> Maybe (Text, Text)
asQualifiedName (Qualified (Just (ModuleName mn)) n) = Just (mn, runIdent n)
asQualifiedName _ = Nothing

evalFunc :: Eval r m => Function EvalAnn -> (Value EvalAnn) -> m (Value EvalAnn)
evalFunc (Function fEnv arg body) param' = do
  env <- view (field @"env")
  let newEnv = (Env fEnv (envForeignFunctions env) <> envBindValue (Qualified Nothing arg) param')
  local (field @"env" .~ newEnv) (eval body)

evalCaseAlts :: Eval r m => SourceSpan -> [(Value EvalAnn)] -> [CaseAlternative EvalAnn] -> m (Value EvalAnn)
evalCaseAlts ss vals [] = throwError (UnexpectedError (Just ss) (prettyText ("Non-exhaustive case expression on values:" <+> pretty vals)))
evalCaseAlts ss values (CaseAlternative binders result : rest) =
  case envFromBinders (zip binders values) of
    Just bindersEnv ->
      case result of
        Left guardedExprs ->
          local (field @"env" %~ (<> bindersEnv)) (evalGuards guardedExprs) >>= \case
            Just expr -> pure expr
            Nothing -> evalCaseAlts ss values rest
        Right expr -> local (field @"env" %~ (<> bindersEnv)) (eval expr)
    Nothing -> evalCaseAlts ss values rest

evalGuards :: Eval r m => [(Guard EvalAnn, Expr EvalAnn)] -> m (Maybe (Value EvalAnn))
evalGuards [] = pure Nothing
evalGuards ((guard', branch) : rest') = do
  res <- require (exprSourceSpan guard') (Proxy @"VBool") =<< eval guard'
  if res then Just <$> eval branch else evalGuards rest'

envFromBinders :: [(Binder EvalAnn, (Value EvalAnn))] -> Maybe (Env m EvalAnn)
envFromBinders = fmap fold . traverse envFromBinder
  where
    envFromBinder :: (Binder EvalAnn, (Value EvalAnn)) -> Maybe (Env m EvalAnn)
    envFromBinder = \case
      (NullBinder _, _) -> Just mempty
      (LiteralBinder _ lit, val) ->
        case (lit, val) of
          (NumericLiteral (Left n1), VInt n2) | fromInteger n1 == n2 -> Just mempty
          (StringLiteral (decodeString -> Just s1), VString s2) | s1 == s2 -> Just mempty
          (CharLiteral c1, VChar c2) | c1 == c2 -> Just mempty
          (BooleanLiteral b1, VBool b2) | b1 == b2 -> Just mempty
          (ArrayLiteral bs, VArray vs)
            | length bs <= length vs ->
              envFromBinders (zip bs (Vector.toList vs))
          (ObjectLiteral bs, VObject vs) -> do
            envs <- for bs $ \(k, binder) -> do
              k' <- decodeString k
              v <- HashMap.lookup k' vs
              envFromBinder (binder, v)
            pure (fold envs)
          _ -> Nothing
      (VarBinder _ n, v) -> Just (envBindValue (Qualified Nothing n) v)
      (NamedBinder _ n b, v) -> do
        env' <- envFromBinder (b, v)
        pure (env' <> envBindValue (Qualified Nothing n) v)
      (ConstructorBinder (EvalAnn _ (Just IsNewtype) _) _typeName _ [b], val) ->
        envFromBinder (b, val)
      (ConstructorBinder _ _typeName (Qualified _ ctorName) bs, val) -> do
        VObject obj <- pure val
        VString ctor <- HashMap.lookup "constructor" obj
        VArray fields <- HashMap.lookup "fields" obj
        if ctor == runProperName ctorName
          then envFromBinders (zip bs (Vector.toList fields))
          else Nothing

exprSourceSpan :: CF.Expr EvalAnn -> SourceSpan
exprSourceSpan = annSourceSpan . CF.extractAnn

prettyText :: Doc ann -> Text
prettyText x = renderStrict (layoutPretty defaultLayoutOptions x)
