{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Version (Version)
import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString, mkString)
import Protolude hiding (moduleName)
import WTP.Element
import System.FilePath.Glob (glob)

data Value
  = VNull
  | VBool Bool
  | VElement Element
  | VString Text
  | VChar Char
  | VNumber Scientific
  | VArray (Vector Value)
  | VObject (HashMap Text Value)
  | VFunction (Ident, (Expr Ann))
  deriving (Show, Generic)

data EvalError
  = UnexpectedError Text
  | EntryPointNotDefined (Qualified Ident)
  | NotInScope SourceSpan (Qualified Ident)
  | InvalidString SourceSpan
  deriving (Eq, Show)

type Eval a = Except EvalError a

runEval :: Eval a -> (Either EvalError a)
runEval = runExcept


require ::
  forall (ctor :: Symbol) s t a b.
  (KnownSymbol ctor, AsConstructor ctor s t a b, s ~ Value, t ~ Value, a ~ b) =>
  Proxy ctor ->
  Value ->
  Eval b
require (ctor :: Proxy ctor) v = case v ^? _Ctor @ctor of
  Just x -> pure x
  Nothing ->
    throwError
      ( UnexpectedError
          ( "Expected value of type: "
              <> Text.toLower (Text.drop 1 (Text.pack (symbolVal ctor)))
          )
      )

evalString :: SourceSpan -> PSString -> Eval Text
evalString ss s =
  case decodeString s of
    Just t -> pure t
    Nothing -> throwError (InvalidString ss)

data Env = Env
  { locals ::  Map Ident Value
  , topLevel :: Map (Qualified Ident) (Expr Ann)
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
withoutLocals env = env { locals = mempty }

envLookup :: Qualified Ident -> Env -> Maybe (Either Value (Expr Ann))
envLookup qn@(Qualified Nothing n) env =
  (Left <$> Map.lookup n (locals env)) <|> (Right <$> Map.lookup qn (topLevel env))
envLookup qn env = (Right <$> Map.lookup qn (topLevel env))

envLookupEval :: SourceSpan -> Qualified Ident -> Env -> Eval Value
envLookupEval ss qn env =
  case envLookup qn env of
    Just r -> either pure (eval (withoutLocals env)) r
    Nothing -> throwError (NotInScope ss qn)

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
        obj <- require (Proxy @"VObject") =<< eval env expr
        maybe
          (throwError (UnexpectedError ("Key not present in object: " <> key)))
          pure
          (HashMap.lookup key obj)
      ObjectUpdate (ss, _, _, _) expr updates -> do
        obj <- require (Proxy @"VObject") =<< eval env expr
        updates' <- for updates $ \(field, expr') ->
          (,) <$> evalString ss field <*> eval env expr'
        pure (VObject (obj <> HashMap.fromList updates'))
      Abs _ arg body -> pure (VFunction (arg, body))
      App _ func param -> do
        (arg, body) <- require (Proxy @"VFunction") =<< eval env func
        param' <- eval env param
        eval (env <> newLocal arg param') body
      Var (ss, _, _, Just IsForeign) qn -> traceShow qn $ envLookupEval ss qn env
      Var (ss, _, _, _) qn -> envLookupEval ss qn env
      Case _ _exprs _alts -> throwError (UnexpectedError "Case expressions are not yet supported")
      Let _ bindings body -> do
        let evalBinding = \case
              NonRec _ name expr -> newLocal name <$> eval env expr
              Rec _group -> throwError (UnexpectedError "Mutually recursive let bindings are not yet supported")
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
    Right env ->  do
      print (runEval (evalEntryPoint entry env))
    Left err -> putStrLn err
