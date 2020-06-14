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
import qualified Data.Aeson as JSON
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Scientific
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Vector as Vector
import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn hiding (Ann)
import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString, mkString)
import Protolude hiding (Meta, moduleName)
import qualified WebCheck.Element as WebCheck
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Value
import qualified WebCheck.Trace as WebCheck

class (MonadError EvalError m, MonadFix m) => MonadEval m where
  getEnv :: env ~ Env EvalAnn => m env
  modifyEnv :: env ~ Env EvalAnn => (env -> env) -> m a -> m a

  eval :: MonadEval m => Expr EvalAnn -> m (Value EvalAnn)

pattern BuiltIn :: Text -> a -> Expr a -> Expr a
pattern BuiltIn name ann p <- CF.App ann (CF.Var _ (Qualified (Just (ModuleName "WebCheck.DSL")) (Ident name))) p

pattern Always :: a -> Expr a -> Expr a
pattern Always ann p <- BuiltIn "always" ann p

pattern Next :: a -> Expr a -> Expr a
pattern Next ann p <- BuiltIn "next" ann p

data ApplyForeign = ApplyForeign (Qualified Ident) [Ident]
  deriving (Show, Generic)

data EvalAnn = EvalAnn {annSourceSpan :: SourceSpan, annMeta :: Maybe Meta, annApplyForeign :: Maybe ApplyForeign}
  deriving (Show, Generic)

evalAnnFromAnn :: CF.Ann -> EvalAnn
evalAnnFromAnn (ss, _, _, meta) = EvalAnn ss meta Nothing

type EvalForeignFunction m = SourceSpan -> [Value EvalAnn] -> m (Value EvalAnn)

data EvalEnv m
  = EvalEnv
      { env :: Env EvalAnn,
        observedStates :: [(Int, WebCheck.ObservedState)],
        foreignFunctions :: Map (Qualified Ident) (EvalForeignFunction m)
      }
  deriving (Generic)

newtype Eval m a = Eval (ExceptT EvalError (ReaderT (EvalEnv (Eval m)) m) a)
  deriving (Functor, Applicative, Monad, MonadError EvalError, MonadFix, MonadReader (EvalEnv (Eval m)))

runEval ::
  Env EvalAnn ->
  [WebCheck.ObservedState] ->
  Map (Qualified Ident) (EvalForeignFunction (Eval m)) ->
  Eval m a ->
  m (Either EvalError a)
runEval env observedStates ffs (Eval ma) = runReaderT (runExceptT ma) (EvalEnv env (zip [1 ..] observedStates) ffs)

instance (Monad m, MonadFix m) => MonadEval (Eval m) where

  getEnv = asks (view (field @"env"))
  modifyEnv f = local (field @"env" %~ f)

  eval expr = do
    EvalEnv {env, observedStates, foreignFunctions} <- ask
    case observedStates of
      [] -> case expr of
        Always _ _ -> pure (VBool True)
        _ -> throwError Undetermined
      ((_n, current) : rest) -> case expr of
        -- Special cases
        Next _ p -> local (field @"observedStates" .~ rest) (eval p)
        Always ann p -> do
          first' <- require (exprSourceSpan p) (Proxy @"VBool")
            =<< eval p `catchError` \case
              Undetermined -> pure (VBool True)
              e -> throwError e
          rest' <-
            require (annSourceSpan ann) (Proxy @"VBool")
              =<< local (field @"observedStates" %~ drop 1) (eval expr)
          pure (VBool (first' && rest'))
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
        App _ (BuiltIn "_queryAll" _ p) q -> evalQuery p q current
        BuiltIn "_property" _ p -> do
          name <- require (exprSourceSpan p) (Proxy @"VString") =<< eval p
          pure (VElementState (WebCheck.Property name))
        BuiltIn "_attribute" _ p -> do
          name <- require (exprSourceSpan p) (Proxy @"VString") =<< eval p
          pure (VElementState (WebCheck.Attribute name))
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
        Abs _ann arg body -> pure (VFunction (Function env arg body))
        App _ func param -> do
          func' <- require (exprSourceSpan func) (Proxy @"VFunction") =<< eval func
          param' <- eval param
          evalFunc func' param'
        Var _ (Qualified (Just (ModuleName "Prim")) (Ident "undefined")) -> pure (VObject mempty)
        Var (EvalAnn ss _ (Just (ApplyForeign qn names))) _ -> do
          params <- for names $ \n -> envLookupEval ss (Qualified Nothing n)
          case Map.lookup qn foreignFunctions of
            Just f -> f ss params
            _ -> throwError (ForeignFunctionNotSupported ss qn)
        Var (EvalAnn ss _ Nothing) qn -> envLookupEval ss qn
        Case (EvalAnn ss _ _) exprs alts -> do
          values <- traverse eval exprs
          evalCaseAlts ss values alts
        Let (EvalAnn _ss _ _) bindings body -> do
          let bindingEnv env'' = \case
                NonRec _ name expr' -> do
                  pure (env'' <> envBindValue (Qualified Nothing name) (VDefer (Defer env'' expr')))
                Rec binds -> do
                  rec recEnv <-
                        fold
                          <$> traverse
                            ( \((_, name), expr') ->
                                envBindValue (Qualified Nothing name) <$> pure (VDefer (Defer (env'' <> recEnv) expr'))
                            )
                            binds
                  pure recEnv
          newEnv <- foldM bindingEnv env bindings
          modifyEnv (<> newEnv) (eval body)


evalString :: Monad m => SourceSpan -> PSString -> Eval m Text
evalString ss s =
  case decodeString s of
    Just t -> pure t
    Nothing -> throwError (InvalidString ss)

evalStringExpr :: Monad m => Expr EvalAnn -> Eval m Text
evalStringExpr (Literal ann (StringLiteral s)) =
  case decodeString s of
    Just t -> pure t
    Nothing -> throwError (InvalidString (annSourceSpan ann))
evalStringExpr expr = throwError (InvalidString (annSourceSpan (extractAnn expr)))

envLookupEval :: (MonadEval m, MonadFix m) => SourceSpan -> Qualified Ident -> m (Value EvalAnn)
envLookupEval ss qn = do
  env' <- getEnv
  case envLookup qn env' of
    Just r -> either (modifyEnv withoutLocals . eval) onValue r
    Nothing -> throwError (NotInScope ss qn)
  where
    onValue (VDefer (Defer env'' expr')) = modifyEnv (const env'') (eval expr')
    onValue val = pure val

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

evalFunc :: MonadEval m => Function EvalAnn -> (Value EvalAnn) -> m (Value EvalAnn)
evalFunc (Function fEnv arg body) param' =
  let newEnv = (fEnv <> envBindValue (Qualified Nothing arg) param')
   in modifyEnv (const newEnv) (eval body)

evalQuery :: MonadFix m => Expr EvalAnn -> Expr EvalAnn -> WebCheck.ObservedState -> Eval m (Value EvalAnn)
evalQuery p1 p2 (WebCheck.ObservedState current) = do
  selector <- require (exprSourceSpan p1) (Proxy @"VString") =<< eval p1
  wantedStates <- require (exprSourceSpan p2) (Proxy @"VObject") =<< eval p2
  matchedElements <-
    maybe
      (throwError (ForeignFunctionError (Just (exprSourceSpan p1)) ("Selector not in observed state: " <> selector)))
      pure
      (HashMap.lookup (WebCheck.Selector selector) current)
  mappedElements <- for (Vector.fromList matchedElements) $ \matchedElement -> do
    mappings <- flip HashMap.traverseWithKey wantedStates $ \k s -> do
      elementState <- require (exprSourceSpan p2) (Proxy @"VElementState") s
      case HashMap.lookup elementState matchedElement of
        Just x -> pure (fromValue x)
        Nothing ->
          let msg = ("Element state (bound to ." <> k <> ") not in observed state for query `" <> selector <> "`: " <> show elementState)
           in throwError (ForeignFunctionError (Just (exprSourceSpan p2)) msg)
    pure (VObject mappings)
  pure (VArray mappedElements)
  where
    fromValue = \case
      JSON.Null -> VObject mempty
      JSON.Bool b -> VBool b
      JSON.String t -> VString t
      JSON.Number n -> either VNumber VInt (floatingOrInteger n)
      JSON.Array xs -> VArray (map fromValue xs)
      JSON.Object xs -> VObject (map fromValue xs)

evalCaseAlts :: MonadEval m => SourceSpan -> [(Value EvalAnn)] -> [CaseAlternative EvalAnn] -> m (Value EvalAnn)
evalCaseAlts ss vals [] = throwError (UnexpectedError (Just ss) (prettyText ("Non-exhaustive case expression on values:" <+> pretty vals)))
evalCaseAlts ss values (CaseAlternative binders result : rest) =
  case envFromBinders (zip binders values) of
    Just bindersEnv ->
      case result of
        Left guardedExprs ->
          modifyEnv (<> bindersEnv) (evalGuards guardedExprs) >>= \case
            Just expr -> pure expr
            Nothing -> evalCaseAlts ss values rest
        Right expr -> modifyEnv (<> bindersEnv) (eval expr)
    Nothing -> evalCaseAlts ss values rest

evalGuards :: MonadEval m => [(Guard EvalAnn, Expr EvalAnn)] -> m (Maybe (Value EvalAnn))
evalGuards [] = pure Nothing
evalGuards ((guard', branch) : rest') = do
  res <- require (exprSourceSpan guard') (Proxy @"VBool") =<< eval guard'
  if res then Just <$> eval branch else evalGuards rest'

envFromBinders :: [(Binder EvalAnn, (Value EvalAnn))] -> Maybe (Env EvalAnn)
envFromBinders = fmap fold . traverse envFromBinder
  where
    envFromBinder :: (Binder EvalAnn, (Value EvalAnn)) -> Maybe (Env EvalAnn)
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