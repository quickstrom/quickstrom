{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WTP.PureScript where

import Control.Lens hiding (op)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Generics.Sum (AsConstructor, _Ctor)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Scientific
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Version (Version)
import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString)
import Protolude
import WTP.Element

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
  | EntryPointNotDefined Ident
  | NotInScope (Qualified Ident)
  | InvalidString SourceSpan
  deriving (Eq, Show)

type Eval a = Except EvalError a

runEval :: Eval a -> (Either EvalError a)
runEval = runExcept

loadModule :: FilePath -> IO (Either Text (Version, Module Ann))
loadModule path = do
  j <- liftIO (BS.readFile path)
  case JSON.decode j of
    Just val ->
      case JSON.parse moduleFromJSON val of
        JSON.Success m -> pure (Right m)
        JSON.Error e -> pure (Left (toS e))
    Nothing -> pure (Left "Couldn't read CoreFn file.")

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

findBind :: Module a -> Ident -> Maybe (Expr a)
findBind m ident = head (catMaybes (map fromBind (moduleDecls m)))
  where
    fromBind :: Bind a -> Maybe (Expr a)
    fromBind = \case
      NonRec _ name expr | name == ident -> pure expr
      _ -> Nothing

evalString :: SourceSpan -> PSString -> Eval Text
evalString ss s =
  case decodeString s of
    Just t -> pure t
    Nothing -> throwError (InvalidString ss)

type LocalEnv = HashMap Text Value

eval :: LocalEnv -> Expr Ann -> Eval Value
eval = go
  where
    go locals = \case
      Literal (ss, _, _, _) lit -> case lit of
        NumericLiteral n -> pure (either (VNumber . fromIntegral) (VNumber . realToFrac) n)
        StringLiteral s -> VString <$> evalString ss s
        CharLiteral c -> pure (VChar c)
        BooleanLiteral b -> pure (VBool b)
        ArrayLiteral xs -> VArray . Vector.fromList <$> traverse (go locals) xs
        ObjectLiteral pairs -> do
          pairs' <- for pairs $ \(field, value) ->
            (,) <$> evalString ss field <*> go locals value
          pure (VObject (HashMap.fromList pairs'))
      Constructor _ typeName ctorName fieldNames ->
        throwError (UnexpectedError "Constructors are not yet supported")
      Accessor (ss, _, _, _) prop expr -> do
        key <- evalString ss prop
        obj <- require (Proxy @"VObject") =<< go locals expr
        maybe
          (throwError (UnexpectedError ("Key not present in object: " <> key)))
          pure
          (HashMap.lookup key obj)
      ObjectUpdate (ss, _, _, _) expr updates -> do
        obj <- require (Proxy @"VObject") =<< go locals expr
        updates' <- for updates $ \(field, expr') ->
          (,) <$> evalString ss field <*> go locals expr'
        pure (VObject (obj <> HashMap.fromList updates'))
      Abs _ arg body -> pure (VFunction (arg, body))
      App _ func param -> do
        (arg, body) <- require (Proxy @"VFunction") =<< go locals func
        param' <- go locals param
        go (locals <> HashMap.singleton (runIdent arg) param') body
      Var _ qn@(Qualified Nothing localIdent) ->
        maybe (throwError (NotInScope qn)) pure (HashMap.lookup (runIdent localIdent) locals)
      Var _ qn@(Qualified _ _) ->
        throwError (NotInScope qn)
      Case _ exprs alts -> throwError (UnexpectedError "Case expressions are not yet supported")
      Let _ bindings body -> do
        let evalBinding = \case
              NonRec _ name expr -> (runIdent name,) <$> go locals expr
              Rec group -> throwError (UnexpectedError "Mutually recursive let bindings are not yet supported")
        newEnv <- HashMap.fromList <$> traverse evalBinding bindings
        go (locals <> newEnv) body
        
              
      

evalEntryPoint :: Module Ann -> Ident -> Eval Value
evalEntryPoint m entryPoint =
  case findBind m entryPoint of
    Just entry -> eval mempty entry
    Nothing -> throwError (EntryPointNotDefined entryPoint)

test :: FilePath -> Text -> IO ()
test path name = do
  loadModule path >>= \case
    Right (_v, m) -> print (runEval (evalEntryPoint m (Ident name)))
    Left e -> putStrLn e
