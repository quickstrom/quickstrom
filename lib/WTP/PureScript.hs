{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WTP.PureScript where

import Protolude

import Language.PureScript.CoreFn
import Language.PureScript.Names
import WTP.Element
import Data.Scientific
import Data.HashSet (HashSet)
import Data.Vector (Vector)
import Data.Version (Version)
import qualified Data.Aeson.Types as JSON
import qualified Data.Aeson as JSON
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import qualified Data.ByteString.Lazy.Char8 as BS

data Value
  = VNull
  | VBool Bool
  | VElement Element
  | VString Text
  | VFunction Name Value
  | VVar Name
  | VNumber Scientific
  | VSeq (Vector Value)
  | VSet (HashSet Value)
  deriving (Eq, Show)


data EvalError
  = Unexpected
  | InvalidEntryPoint Ident
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
        JSON.Error e   -> pure (Left (toS e))
    Nothing -> pure (Left "Couldn't read CoreFn file.")

findBind :: Module a -> Ident -> Maybe (Expr a)
findBind m ident = head (catMaybes (map fromBind (moduleDecls m)))
  where
    fromBind :: Bind a -> Maybe (Expr a)
    fromBind = \case
      NonRec _ name expr | name == ident -> pure expr
      _ -> Nothing

eval :: Module Ann -> Expr Ann -> Eval Value
eval _m = \case
  Literal _ lit -> _
  Constructor _ typeName ctorName fieldNames -> _
  Accessor _ prop expr -> _
  ObjectUpdate _ obj updates -> _
  Abs _ arg body -> _
  App _ func param -> _
  Var _ ident -> _
  Case _ exprs alts -> _
  Let _ bindings body -> _

evalEntryPoint :: Module Ann -> Ident -> Eval Value
evalEntryPoint m entryPoint =
  case findBind m entryPoint of
    Just entry -> eval m entry
    Nothing -> throwError (InvalidEntryPoint entryPoint)


test :: FilePath -> IO ()
test path = do
  loadModule path >>= \case
    Right (_v, m) -> print (runEval (evalEntryPoint m (Ident "spec")))
    Left e -> putStrLn e
