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

module Quickstrom.PureScript.Eval.Interpret where

import Control.Lens hiding (op)
import Control.Monad.Fix (MonadFix)
import Data.Generics.Product (HasField, field)
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Vector as Vector
import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn hiding (Ann)
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Names as P
import Language.PureScript.PSString (PSString, decodeString, mkString)
import qualified Quickstrom.Element as Quickstrom
import Quickstrom.Prelude hiding (HasField)
import Quickstrom.PureScript.Eval.Ann
import Quickstrom.PureScript.Eval.Class
import Quickstrom.PureScript.Eval.Env
import Quickstrom.PureScript.Eval.Error
import Quickstrom.PureScript.Eval.Name
import Quickstrom.PureScript.Value


pattern BuiltIn :: Text -> a -> Expr a -> Expr a
pattern BuiltIn name ann p <- CF.App ann (CF.Var _ (P.Qualified (Just (P.ModuleName "Quickstrom")) (P.Ident name))) p

pattern BinaryBuiltIn :: Text -> a -> Expr a -> Expr a -> Expr a
pattern BinaryBuiltIn name ann p q <- CF.App _ (BuiltIn name ann p) q

pattern Always :: a -> Expr a -> Expr a
pattern Always ann p <- BuiltIn "always" ann p

pattern Next :: a -> Expr a -> Expr a
pattern Next ann p <- BuiltIn "next" ann p

pattern Until :: a -> Expr a -> Expr a -> Expr a
pattern Until ann p q <- BinaryBuiltIn "until" ann p q

type Env' m = Env Expr Value (EvalForeignFunction m) EvalAnn

newtype EvalForeignFunction m ann = EvalForeignFunction (SourceSpan -> [Value ann] -> m (Value ann))

type Eval r m =
  ( MonadFix m,
    MonadReader r m,
    HasField "env" r r (Env' m) (Env' m),
    MonadEvalQuery m
  )

{-# SCC eval "eval" #-}
eval :: Eval r m => Expr EvalAnn -> m (Value EvalAnn)
eval expr = do
  env <- view (field @"env") <$> ask
  case expr of
    Next ann p -> evalNext ann p
    Always ann p -> evalAlways ann p
    Until ann p q -> evalUntil ann p q
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
      pure (VElementState (Quickstrom.Property name))
    BuiltIn "_attribute" _ p -> do
      name <- require (exprSourceSpan p) (Proxy @"VString") =<< eval p
      pure (VElementState (Quickstrom.Attribute name))
    BuiltIn "cssValue" _ p -> do
      name <- require (exprSourceSpan p) (Proxy @"VString") =<< eval p
      pure (VElementState (Quickstrom.CssValue name))
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
      pure (VFunction (Function mempty (P.Ident "value") (Var (EvalAnn ss Nothing Nothing) (unqualifiedName "value"))))
    Constructor ann _typeName ctorName fieldNames -> do
      let body =
            Literal
              ann
              ( ObjectLiteral
                  [ (mkString "constructor", Literal ann (StringLiteral (mkString (P.runProperName ctorName)))),
                    (mkString "fields", Literal ann (ArrayLiteral (map (Var ann . P.Qualified Nothing) fieldNames)))
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
    Abs _ann arg body -> pure (VFunction (Function (closureEnvFromEnv env) arg body))
    App _ func param -> do
      func' <- require (exprSourceSpan func) (Proxy @"VFunction") =<< eval func
      param' <- eval param
      evalFunc func' param'
    Var _ (P.Qualified (Just (P.ModuleName "Prim")) (P.Ident "undefined")) -> pure (VObject mempty)
    Var (EvalAnn ss _ (Just (ApplyForeign qn names))) _ -> do
      params <- for names $ \n -> envLookupEval ss (Right n)
      case HashMap.lookup qn (envForeignFunctions env) of
        Just (EvalForeignFunction f) -> f ss params
        _ -> throwError (ForeignFunctionNotSupported ss qn)
    Var (EvalAnn ss _ Nothing) qn -> envLookupEval ss (fromQualifiedIdent qn)
    Case (EvalAnn ss _ _) exprs alts -> do
      values <- traverse eval exprs
      evalCaseAlts ss values alts
    Let (EvalAnn _ss _ _) bindings body -> do
      let bindingEnv (env'') = \case
            NonRec _ name expr' -> do
              pure (env'' <> envBindLocal (fromIdent name) (VDefer (Defer (closureEnvFromEnv env'') expr')))
            Rec binds -> do
              rec recEnv <-
                    fold
                      <$> traverse
                        ( \((_, name), expr') ->
                            envBindLocal (fromIdent name) <$> pure (VDefer (Defer (closureEnvFromEnv (env'' <> recEnv)) expr'))
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

envLookupEval :: (Eval r m) => SourceSpan -> Either QualifiedName Name -> m (Value EvalAnn)
envLookupEval ss n = do
  env' <- view (field @"env")
  let onValue (VDefer (Defer env'' expr')) = local (field @"env" .~ env'' {envForeignFunctions = envForeignFunctions env'}) (eval expr')
      onValue val = pure val
  case n of
    Left (flip envLookupTopLevel env' -> Just expr) -> local (field @"env" %~ withoutLocals) (eval expr)
    Right (flip envLookupLocal env' -> Just v) -> onValue v
    _ -> throwError (NotInScope ss n)

unqualifiedName :: Text -> P.Qualified P.Ident
unqualifiedName localName = P.Qualified Nothing (P.Ident localName)

asQualifiedVar :: Expr EvalAnn -> Maybe (Text, Text)
asQualifiedVar (Var _ qn) = asQualifiedName qn
asQualifiedVar _ = Nothing

asQualifiedName :: P.Qualified P.Ident -> Maybe (Text, Text)
asQualifiedName (P.Qualified (Just (P.ModuleName mn)) n) = Just (mn, P.runIdent n)
asQualifiedName _ = Nothing

evalFunc :: Eval r m => Function EvalAnn -> (Value EvalAnn) -> m (Value EvalAnn)
evalFunc (Function fEnv arg body) param' = do
  env <- view (field @"env")
  let newEnv = fEnv {envForeignFunctions = (envForeignFunctions env)} <> envBindLocal (fromIdent arg) param'
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

envFromBinders :: [(Binder EvalAnn, (Value EvalAnn))] -> Maybe (Env' m)
envFromBinders = fmap fold . traverse envFromBinder
  where
    envFromBinder :: (Binder EvalAnn, (Value EvalAnn)) -> Maybe (Env' m)
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
      (VarBinder _ n, v) -> Just (envBindLocal (fromIdent n) v)
      (NamedBinder _ n b, v) -> do
        env' <- envFromBinder (b, v)
        pure (env' <> envBindLocal (fromIdent n) v)
      (ConstructorBinder (EvalAnn _ (Just IsNewtype) _) _typeName _ [b], val) ->
        envFromBinder (b, val)
      (ConstructorBinder _ _typeName (P.Qualified _ ctorName) bs, val) -> do
        VObject obj <- pure val
        VString ctor <- HashMap.lookup "constructor" obj
        VArray fields <- HashMap.lookup "fields" obj
        if ctor == P.runProperName ctorName
          then envFromBinders (zip bs (Vector.toList fields))
          else Nothing

exprSourceSpan :: CF.Expr EvalAnn -> SourceSpan
exprSourceSpan = annSourceSpan . CF.extractAnn

prettyText :: Doc ann -> Text
prettyText x = renderStrict (layoutPretty defaultLayoutOptions x)
