{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WTP.PureScript where

import Control.Lens hiding (op)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Generics.Sum (AsConstructor, _Ctor)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Map as Map
import Data.Scientific
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString, mkString)
import Protolude hiding (moduleName)
import System.FilePath.Glob (glob)
import WTP.Element (Element)
import qualified WTP.Element as Element

data Value
  = VNull
  | VBool Bool
  | VElement Element
  | VElementState Element.ElementState
  | VString Text
  | VChar Char
  | VNumber Scientific
  | VArray (Vector Value)
  | VObject (HashMap Text Value)
  | VFunction (Env, Ident, (Expr Ann))
  deriving (Show, Generic)

data EvalError
  = UnexpectedError (Maybe SourceSpan) Text
  | EntryPointNotDefined (Qualified Ident)
  | NotInScope SourceSpan (Qualified Ident)
  | InvalidString SourceSpan
  | InvalidBuiltInFunctionApplication SourceSpan (Expr Ann) (Expr Ann)
  deriving (Show, Generic)

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

sourceSpan :: Expr Ann -> SourceSpan
sourceSpan expr = extractAnn expr ^. _1

require ::
  forall (ctor :: Symbol) s t a b.
  (KnownSymbol ctor, AsConstructor ctor s t a b, s ~ Value, t ~ Value, a ~ b) =>
  SourceSpan ->
  Proxy ctor ->
  Value ->
  Eval b
require ss (ctor :: Proxy ctor) v = case v ^? _Ctor @ctor of
  Just x -> pure x
  Nothing -> unexpectedType ss (Text.toLower (Text.drop 1 (Text.pack (symbolVal ctor)))) v

evalString :: SourceSpan -> PSString -> Eval Text
evalString ss s =
  case decodeString s of
    Just t -> pure t
    Nothing -> throwError (InvalidString ss)

evalStringExpr :: Expr Ann -> Eval Text
evalStringExpr (Literal (ss, _, _, _) (StringLiteral s)) =
  case decodeString s of
    Just t -> pure t
    Nothing -> throwError (InvalidString ss)
evalStringExpr expr = unexpectedType (sourceSpan expr) "string" expr

data Env
  = Env
      { locals :: Map Ident Value,
        topLevel :: Map (Qualified Ident) (Expr Ann)
      }
  deriving (Show)

instance Semigroup Env where
  e1 <> e2 = Env (locals e1 <> locals e2) (topLevel e1 <> topLevel e2)

instance Monoid Env where
  mempty = Env mempty mempty

newLocal :: Ident -> Value -> Env
newLocal ident value = Env (Map.singleton ident value) mempty

newTopLevel :: Qualified Ident -> Expr Ann -> Env
newTopLevel qn expr = Env mempty (Map.singleton qn expr)

withoutLocals :: Env -> Env
withoutLocals env = env {locals = mempty}

envLookup :: Qualified Ident -> Env -> Maybe (Either Value (Expr Ann))
envLookup qn@(Qualified Nothing n) env =
  case Map.lookup n (locals env) of
    Just v -> pure (Left v)
    Nothing -> Right <$> Map.lookup qn (topLevel env)
envLookup qn env = (Right <$> Map.lookup qn (topLevel env))

envLookupEval :: SourceSpan -> Qualified Ident -> Env -> Eval Value
envLookupEval ss qn env =
  case envLookup qn env of
    Just r -> either pure (eval (withoutLocals env)) r
    Nothing -> throwError (NotInScope ss qn)

qualifiedName :: [Text] -> Text -> Qualified Ident
qualifiedName moduleNames localName = Qualified (Just (ModuleName (map ProperName moduleNames))) (Ident localName)

asBuiltInName :: Expr Ann -> Maybe ([Text], Text)
asBuiltInName (Var (_, _, _, Just IsForeign) (Qualified (Just (ModuleName pns)) n)) = Just (map runProperName pns, runIdent n)
asBuiltInName _ = Nothing

evalApp :: Env -> Ann -> Expr Ann -> Expr Ann -> Eval Value
evalApp env (ss, _, _, _) func param =
  case (func, param) of
    (asBuiltInName -> Just (["DSL"], "_property"), p) -> do
      traceShowM ("_property", p)
      name <- require (sourceSpan p) (Proxy @"VString") =<< eval env p
      traceM ("Propery name: " <> name)
      pure (VElementState (Element.Property name))
    (asBuiltInName -> Just (["DSL"], "_attribute"), p) -> do
      traceShowM ("_attribute", p)
      name <- require (sourceSpan p) (Proxy @"VString") =<< eval env p
      pure (VElementState (Element.Attribute name))
    (App _ (asBuiltInName -> Just (["DSL"], "_queryAll")) p1, p2) -> do
      traceShowM ("_queryAll", p1, p2)
      _selector <- require (sourceSpan p1) (Proxy @"VString") =<< eval env p1
      _states <- require ss (Proxy @"VObject") =<< eval env p2
      -- TODO: Get state from trace
      pure (VObject mempty)
    _ -> do
      (fEnv, arg, body) <- require ss (Proxy @"VFunction") =<< eval env func
      param' <- eval env param
      eval (env <> fEnv <> newLocal arg param') body

eval :: Env -> Expr Ann -> Eval Value
eval env = \case
  Literal (ss, _, _, _) lit -> case lit of
    NumericLiteral n -> pure (either (VNumber . fromIntegral) (VNumber . realToFrac) n)
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
  Accessor (ss, _, _, _) prop expr -> do
    key <- evalString ss prop
    obj <- require ss (Proxy @"VObject") =<< eval env expr
    maybe
      (throwError (UnexpectedError (Just ss) ("Key not present in object: " <> key)))
      pure
      (HashMap.lookup key obj)
  ObjectUpdate (ss, _, _, _) expr updates -> do
    obj <- require ss (Proxy @"VObject") =<< eval env expr
    updates' <- for updates $ \(field, expr') ->
      (,) <$> evalString ss field <*> eval env expr'
    pure (VObject (obj <> HashMap.fromList updates'))
  Abs _ arg body -> pure (VFunction (env, arg, body))
  App ann func param -> evalApp env ann func param
  Var _ (Qualified (Just (ModuleName [ProperName "Prim"])) (Ident "undefined")) -> pure (VObject mempty)
  Var (ss, _, _, _) qn -> envLookupEval ss qn env
  Case (ss, _, _, _) _exprs _alts -> throwError (UnexpectedError (Just ss) "Case expressions are not yet supported")
  Let (ss, _, _, _) bindings body -> do
    let evalBinding = \case
          NonRec _ name expr -> newLocal name <$> eval env expr
          Rec _group -> throwError (UnexpectedError (Just ss) "Mutually recursive let bindings are not yet supported")
    newEnv <- fold <$> traverse evalBinding bindings
    eval (env <> newEnv) body

toModuleEnv :: Module Ann -> Env
toModuleEnv m =
  let addDecl = \case
        NonRec _ name expr -> newTopLevel (Qualified (Just (moduleName m)) name) expr
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

loadAllModulesEnv :: [FilePath] -> ExceptT Text IO Env
loadAllModulesEnv paths = do
  ms <- traverse loadModule paths
  pure (foldMap toModuleEnv ms)

evalEntryPoint :: Qualified Ident -> Env -> Eval Value
evalEntryPoint entryPoint env =
  case Map.lookup entryPoint (topLevel env) of
    Just entry -> eval env entry
    Nothing -> throwError (EntryPointNotDefined entryPoint)

test :: Text -> Qualified Ident -> IO ()
test g entry = do
  paths <- glob (toS g)
  runExceptT (loadAllModulesEnv paths) >>= \case
    Right env -> do
      case runEval (evalEntryPoint entry env) of
        Right value -> print value
        Left err -> print err
    Left err -> putStrLn err
