{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module WTP.PureScript where

import Control.Lens hiding (op)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Generics.Sum (AsConstructor, _Ctor)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Language.PureScript.AST (spanName, SourceSpan, nullSourceSpan)
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString, mkString)
import Protolude hiding (moduleName)
import System.FilePath.Glob (glob)
import qualified WTP.Element as Element
import WTP.PureScript.Value
import WTP.PureScript.Foreign

data EvalError
  = UnexpectedError (Maybe SourceSpan) Text
  | EntryPointNotDefined (Qualified Ident)
  | NotInScope SourceSpan (Qualified Ident)
  | InvalidString SourceSpan
  | InvalidBuiltInFunctionApplication SourceSpan (Expr EvalAnn) (Expr EvalAnn)
  deriving (Show, Generic)

data EvalAnn = EvalAnn {annSourceSpan :: SourceSpan, annApplyForeign :: Maybe ApplyForeign}
  deriving (Show, Generic)

evalAnnFromAnn :: Ann -> EvalAnn
evalAnnFromAnn (ss, _, _, _) = EvalAnn ss Nothing

type Eval a = Except EvalError a

runEval :: Eval a -> (Either EvalError a)
runEval = runExcept

unexpectedType :: (MonadError EvalError m, Show value) => SourceSpan -> Text -> value -> m a
unexpectedType ss typ v =
  throwError
    ( UnexpectedError
        (Just ss)
        ( "Expected value of type "
            <> typ
            <> " but got "
            <> show v
        )
    )

sourceSpan :: Expr EvalAnn -> SourceSpan
sourceSpan = annSourceSpan . extractAnn

require ::
  forall (ctor :: Symbol) s t a b ann.
  (KnownSymbol ctor, AsConstructor ctor s t a b, s ~ Value ann, t ~ Value ann, a ~ b, Show ann) =>
  SourceSpan ->
  Proxy ctor ->
  Value ann ->
  Eval b
require ss (ctor :: Proxy ctor) v = case v ^? _Ctor @ctor of
  Just x -> pure x
  Nothing -> unexpectedType ss (Text.toLower (Text.drop 1 (Text.pack (symbolVal ctor)))) v

evalString :: SourceSpan -> PSString -> Eval Text
evalString ss s =
  case decodeString s of
    Just t -> pure t
    Nothing -> throwError (InvalidString ss)

evalStringExpr :: Expr EvalAnn -> Eval Text
evalStringExpr (Literal ann (StringLiteral s)) =
  case decodeString s of
    Just t -> pure t
    Nothing -> throwError (InvalidString (annSourceSpan ann))
evalStringExpr expr = unexpectedType (sourceSpan expr) "string" expr

initialEnv :: Env EvalAnn
initialEnv = fold [foreignFunction ["Data", "Array"] "indexImpl" 4]
  where
    foreignFunction :: [Text] -> Text -> Int -> Env EvalAnn
    foreignFunction ms n arity =
      let qn = (qualifiedName ms n)
       in envBindExpr qn (wrap arity (\names -> Var (EvalAnn nullSourceSpan { spanName = toS (showQualified runIdent qn) } (Just (ApplyForeign qn names))) qn))
    wrap :: Int -> ([Ident] -> Expr EvalAnn) -> Expr EvalAnn
    wrap arity f =
      let names = [Ident ("x" <> show n) | n <- [1 .. arity]]
       in foldr (Abs (EvalAnn nullSourceSpan Nothing)) (f names) names

envLookupEval :: SourceSpan -> Qualified Ident -> Env EvalAnn -> Eval (Value EvalAnn)
envLookupEval ss qn env =
  case envLookup qn env of
    Just r -> either (eval (withoutLocals env)) pure r
    Nothing -> throwError (NotInScope ss qn)

qualifiedName :: [Text] -> Text -> Qualified Ident
qualifiedName moduleNames localName = Qualified (Just (ModuleName (map ProperName moduleNames))) (Ident localName)

asQualifiedVar :: Expr EvalAnn -> Maybe ([Text], Text)
asQualifiedVar (Var _ qn) = asQualifiedName qn
asQualifiedVar _ = Nothing

asQualifiedName :: Qualified Ident -> Maybe ([Text], Text)
asQualifiedName (Qualified (Just (ModuleName pns)) n) = Just (map runProperName pns, runIdent n)
asQualifiedName _ = Nothing

eval :: Env EvalAnn -> Expr EvalAnn -> Eval (Value EvalAnn)
eval env = \case
  Literal (EvalAnn ss _) lit -> case lit of
    NumericLiteral n -> pure (either VInt (VNumber . realToFrac) n)
    StringLiteral s -> VString <$> evalString ss s
    CharLiteral c -> pure (VChar c)
    BooleanLiteral b -> pure (VBool b)
    ArrayLiteral xs -> VArray . Vector.fromList <$> traverse (eval env) xs
    ObjectLiteral pairs -> do
      pairs' <- for pairs $ \(field, value) ->
        (,) <$> evalString ss field <*> eval env value
      pure (VObject (HashMap.fromList pairs'))
  Constructor ann _typeName ctorName fieldNames -> do
    let body =
          Literal
            ann
            ( ObjectLiteral
                [ (mkString "constructor", Literal ann (StringLiteral (mkString (runProperName ctorName)))),
                  (mkString "fields", Literal ann (ArrayLiteral (map (Var ann . Qualified Nothing) fieldNames)))
                ]
            )
    eval env (foldr (Abs ann) body fieldNames)
  -- throwError (UnexpectedError "Constructors are not yet supported")
  Accessor (EvalAnn ss _) prop expr -> do
    key <- evalString ss prop
    obj <- require ss (Proxy @"VObject") =<< eval env expr
    maybe
      (throwError (UnexpectedError (Just ss) ("Key not present in object: " <> key)))
      pure
      (HashMap.lookup key obj)
  ObjectUpdate (EvalAnn ss _) expr updates -> do
    obj <- require ss (Proxy @"VObject") =<< eval env expr
    updates' <- for updates $ \(field, expr') ->
      (,) <$> evalString ss field <*> eval env expr'
    pure (VObject (obj <> HashMap.fromList updates'))
  Abs _ arg body -> pure (VFunction (Function env arg body))
  App ann func param -> evalApp env ann func param
  Var _ (Qualified (Just (ModuleName [ProperName "Prim"])) (Ident "undefined")) -> pure (VObject mempty)
  Var (EvalAnn ss (Just applyForeign)) _ -> evalForeign ss env applyForeign
  Var (EvalAnn ss Nothing) qn -> envLookupEval ss qn env
  Case (EvalAnn ss _) exprs alts -> do
    values <- traverse (eval env) exprs
    evalCaseAlts ss env values alts
  Let (EvalAnn ss _) bindings body -> do
    let evalBinding = \case
          NonRec _ name expr -> envBindValue (Qualified Nothing name) <$> eval env expr
          Rec _group -> throwError (UnexpectedError (Just ss) "Mutually recursive let bindings are not yet supported")
    newEnv <- fold <$> traverse evalBinding bindings
    eval (env <> newEnv) body

evalForeign :: SourceSpan -> Env EvalAnn -> ApplyForeign -> Eval (Value EvalAnn)
evalForeign ss env (ApplyForeign qn paramNames) = do
  params <- for paramNames $ \n -> envLookupEval ss (Qualified Nothing n) env
  case (asQualifiedName qn, params) of
    (Just (["Data", "Array"], "indexImpl"), [just, nothing, xs, i]) -> do
      just' <- require ss (Proxy @"VFunction") just
      xs' <- require ss (Proxy @"VArray") xs
      i' <- require ss (Proxy @"VInt") i
      case xs' ^? ix (fromIntegral i') of
        Just x -> evalFunc env just' x
        Nothing -> pure nothing
    _ -> pure VNull

evalApp :: Env EvalAnn -> EvalAnn -> Expr EvalAnn -> Expr EvalAnn -> Eval (Value EvalAnn)
evalApp env ann func param =
  case (func, param) of
    (asQualifiedVar -> Just (["DSL"], "_property"), p) -> do
      name <- require (sourceSpan p) (Proxy @"VString") =<< eval env p
      pure (VElementState (Element.Property name))
    (asQualifiedVar -> Just (["DSL"], "_attribute"), p) -> do
      name <- require (sourceSpan p) (Proxy @"VString") =<< eval env p
      pure (VElementState (Element.Attribute name))
    (App _ (asQualifiedVar -> Just (["DSL"], "_queryAll")) p1, p2) -> do
      _selector <- require (sourceSpan p1) (Proxy @"VString") =<< eval env p1
      _states <- require (annSourceSpan ann) (Proxy @"VObject") =<< eval env p2
      -- TODO: Get state from trace
      pure (VArray mempty)
    _ -> do
      func' <- require (sourceSpan func) (Proxy @"VFunction") =<< eval env func
      param' <- eval env param
      evalFunc env func' param'

evalFunc :: Env EvalAnn -> Function EvalAnn -> (Value EvalAnn) -> Eval (Value EvalAnn)
evalFunc env (Function fEnv arg body) param' =
  let newEnv = (env <> fEnv <> envBindValue (Qualified Nothing arg) param')
   in eval newEnv body

evalCaseAlts :: SourceSpan -> Env EvalAnn -> [(Value EvalAnn)] -> [CaseAlternative EvalAnn] -> Eval (Value EvalAnn)
evalCaseAlts ss _ _ [] = throwError (UnexpectedError (Just ss) "Non-exhaustive case expression")
evalCaseAlts ss env values (CaseAlternative binders result : rest) =
  case envFromBinders (zip binders values) of
    Just bindersEnv ->
      case result of
        Left guardedExprs ->
          evalGuards (env <> bindersEnv) guardedExprs >>= \case
            Just expr -> pure expr
            Nothing -> evalCaseAlts ss env values rest
        Right expr -> eval (env <> bindersEnv) expr
    Nothing -> evalCaseAlts ss env values rest

evalGuards :: Env EvalAnn -> [(Guard EvalAnn, Expr EvalAnn)] -> Eval (Maybe (Value EvalAnn))
evalGuards _ [] = pure Nothing
evalGuards env ((guard', branch) : rest') = do
  res <- require (sourceSpan guard') (Proxy @"VBool") =<< eval env guard'
  if res then Just <$> eval env branch else evalGuards env rest'

envFromBinders :: [(Binder EvalAnn, (Value EvalAnn))] -> Maybe (Env EvalAnn)
envFromBinders = fmap fold . traverse envFromBinder
  where
    envFromBinder :: (Binder EvalAnn, (Value EvalAnn)) -> Maybe (Env EvalAnn)
    envFromBinder = \case
      (NullBinder _, _) -> Just mempty
      (LiteralBinder _ lit, val) -> 
        case (lit, val) of
          (NumericLiteral (Left n1), VInt n2) | n1 == n2 -> Just mempty
          (StringLiteral (decodeString -> Just s1), VString s2) | s1 == s2 -> Just mempty
          (CharLiteral c1, VChar c2) | c1 == c2 -> Just mempty
          (BooleanLiteral b1, VBool b2) | b1 == b2 -> Just mempty
          (ArrayLiteral bs, VArray vs) | length bs <= length vs -> 
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
      (ConstructorBinder _ _typeName (Qualified _ ctorName) bs, val) -> do
        VObject obj <- pure val
        VString ctor <- HashMap.lookup "constructor" obj 
        VArray fields <- HashMap.lookup "fields" obj 
        if ctor == runProperName ctorName
          then envFromBinders (zip bs (Vector.toList fields))
          else Nothing

toModuleEnv :: Module Ann -> Env EvalAnn
toModuleEnv m =
  let addDecl = \case
        NonRec _ name expr -> envBindExpr (Qualified (Just (moduleName m)) name) (evalAnnFromAnn <$> expr)
        Rec _group -> mempty -- TODO
   in foldMap addDecl (moduleDecls m)

type AllModules = Map ModuleName (Module Ann)

loadModule :: FilePath -> ExceptT Text IO (Module Ann)
loadModule path = do
  j <- liftIO (BS.readFile path)
  case JSON.decode j of
    Just val ->
      case JSON.parse moduleFromJSON val of
        JSON.Success (_, m) -> pure m
        JSON.Error e -> throwError (toS e)
    Nothing -> throwError "Couldn't read CoreFn file."

loadAllModulesEnv :: [FilePath] -> ExceptT Text IO (Env EvalAnn)
loadAllModulesEnv paths = do
  ms <- traverse loadModule paths
  pure (foldMap toModuleEnv ms)

evalEntryPoint :: Qualified Ident -> Env EvalAnn -> Eval (Value EvalAnn)
evalEntryPoint entryPoint env =
  envLookupEval nullSourceSpan entryPoint env

test :: Text -> Qualified Ident -> IO ()
test g entry = do
  paths <- glob (toS g)
  runExceptT (loadAllModulesEnv paths) >>= \case
    Right env -> do
      case runEval (evalEntryPoint entry (initialEnv <> env)) of
        Right value -> print value
        Left err -> print err
    Left err -> putStrLn err
