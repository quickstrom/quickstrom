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
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.PureScript.AST (SourceSpan, nullSourceSpan, spanName)
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString, mkString)
import Protolude hiding (moduleName)
import System.FilePath.Glob (glob)
import qualified WTP.Element as Element
import WTP.PureScript.Value

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
initialEnv =
  foldMap
    (\(qn, (ForeignFunction (_ :: f))) -> foreignFunction qn (foreignFunctionArity (Proxy :: Proxy f)))
    (Map.toList foreignFunctions)
  where
    foreignFunction :: Qualified Ident -> Int -> Env EvalAnn
    foreignFunction qn arity =
      envBindExpr qn (wrap arity (\names -> Var (EvalAnn nullSourceSpan {spanName = toS (showQualified runIdent qn)} (Just (ApplyForeign qn names))) qn))
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
  Var (EvalAnn ss (Just applyForeign)) _ -> evalForeignApply ss env applyForeign
  Var (EvalAnn ss Nothing) qn -> envLookupEval ss qn env
  Case (EvalAnn ss _) exprs alts -> do
    values <- traverse (eval env) exprs
    evalCaseAlts ss env values alts
  Let (EvalAnn ss _) bindings body -> do
    let evalBinding env' = \case
          NonRec _ name expr -> do
            value <- eval env' expr
            pure (env' <> envBindValue (Qualified Nothing name) value)
          Rec _group -> throwError (UnexpectedError (Just ss) "Mutually recursive let bindings are not yet supported")
    newEnv <- foldM evalBinding env bindings
    eval (env <> newEnv) body

evalApp :: Env EvalAnn -> EvalAnn -> Expr EvalAnn -> Expr EvalAnn -> Eval (Value EvalAnn)
evalApp env ann func param =
  case (func, param) of
    (asQualifiedVar -> Just (["DSL"], "next"), p) -> eval env p -- TODO: handle temporal operators
    (asQualifiedVar -> Just (["DSL"], "always"), p) -> eval env p -- TODO: handle temporal operators
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
        NonRec _ name expr -> bindExpr name expr
        Rec binds -> foldMap (\((_, name), expr) -> bindExpr name expr) binds
   in foldMap addDecl (moduleDecls m)
  where
    bindExpr name expr = envBindExpr (Qualified (Just (moduleName m)) name) (evalAnnFromAnn <$> expr)

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
  putStrLn ("Loading " <> show (length paths) <> " modules..." :: Text)
  runExceptT (loadAllModulesEnv paths) >>= \case
    Right env -> do
      case runEval (evalEntryPoint entry (initialEnv <> env)) of
        Right value -> putStrLn (renderStrict (layoutPretty defaultLayoutOptions (pretty value)))
        Left err -> print err
    Left err -> putStrLn err

-- * Foreign Functions

data ApplyForeign = ApplyForeign (Qualified Ident) [Ident]
  deriving (Show, Generic)

data ForeignFunction = forall f. (EvalForeignFunction f, ForeignFunctionArity f) => ForeignFunction f

class ForeignFunctionArity f where
  foreignFunctionArity :: Proxy f -> Int

instance ForeignFunctionArity b => ForeignFunctionArity (a -> b) where
  foreignFunctionArity (_ :: Proxy (a -> b)) = succ (foreignFunctionArity (Proxy :: Proxy b))

instance {-# OVERLAPPABLE #-} ForeignFunctionArity a where
  foreignFunctionArity _ = 0

foreignFunctionArityMismatch :: SourceSpan -> Eval a
foreignFunctionArityMismatch ss = throwError (UnexpectedError (Just ss) "Foreign function arity mismatch")

class EvalForeignFunction f where
  evalForeignFunction :: SourceSpan -> Env EvalAnn -> [Value EvalAnn] -> f -> Eval (Value EvalAnn)

instance (FromForeignValue a, EvalForeignFunction b) => EvalForeignFunction (a -> b) where
  evalForeignFunction ss env (v : vs) f = do
    a <- fromForeignValue ss env v
    evalForeignFunction ss env vs (f a)
  evalForeignFunction ss _ [] _ = foreignFunctionArityMismatch ss

-- instance {-# OVERLAPPABLE #-} ToForeignValue a => EvalForeignFunction a where
--   evalForeignFunction _ _ [] x = pure (toForeignValue x)
--   evalForeignFunction ss _ _ _ = foreignFunctionArityMismatch ss

instance {-# OVERLAPPABLE #-} ToForeignValue a => EvalForeignFunction (Eval a) where
  evalForeignFunction _ _ [] x = toForeignValue <$> x
  evalForeignFunction ss _ _ _ = foreignFunctionArityMismatch ss

instance EvalForeignFunction (Eval (Value EvalAnn)) where
  evalForeignFunction _ _ _ = identity

class FromForeignValue r where
  fromForeignValue :: SourceSpan -> Env EvalAnn -> Value EvalAnn -> Eval r

instance FromForeignValue (Value EvalAnn) where
  fromForeignValue _ _ = pure

instance FromForeignValue a => FromForeignValue (Eval a) where
  fromForeignValue ss env x = do
    fromForeignValue ss env x

instance FromForeignValue Bool where
  fromForeignValue ss _ = require ss (Proxy @"VBool")

instance FromForeignValue Text where
  fromForeignValue ss _ = require ss (Proxy @"VString")

instance FromForeignValue Char where
  fromForeignValue ss _ = require ss (Proxy @"VChar")

instance FromForeignValue Integer where
  fromForeignValue ss _ = require ss (Proxy @"VInt")

instance FromForeignValue Scientific where
  fromForeignValue ss _ = require ss (Proxy @"VNumber")

instance FromForeignValue (Vector (Value EvalAnn)) where
  fromForeignValue ss _ = require ss (Proxy @"VArray")

instance (ToForeignValue a, FromForeignValue b) => FromForeignValue (a -> Eval b) where
  fromForeignValue ss env fn =
    pure (\x -> do
      fn' <- require ss (Proxy @"VFunction") fn
      fromForeignValue ss env =<< evalFunc env fn' (toForeignValue x)
      )

instance (ToForeignValue a, ToForeignValue b, FromForeignValue c) => FromForeignValue (a -> b -> Eval c) where
  fromForeignValue ss env fn = do
    pure (\a b -> do
      fn' <- require ss (Proxy @"VFunction") fn
      fn'' <- require ss (Proxy @"VFunction") =<< evalFunc env fn' (toForeignValue a)
      fromForeignValue ss env =<< evalFunc env fn'' (toForeignValue b)
      )

class ToForeignValue a where
  toForeignValue :: a -> Value EvalAnn

instance ToForeignValue Bool where
  toForeignValue = VBool

instance ToForeignValue Integer where
  toForeignValue = VInt

instance ToForeignValue Scientific where
  toForeignValue = VNumber

instance ToForeignValue Char where
  toForeignValue = VChar

instance ToForeignValue Text where
  toForeignValue = VString

instance ToForeignValue a => ToForeignValue (Vector a) where
  toForeignValue xs = VArray (toForeignValue <$> xs)

instance ToForeignValue (Value EvalAnn) where
  toForeignValue = identity

foreignFunctions :: Map (Qualified Ident) ForeignFunction
foreignFunctions =
  Map.fromList
    [ (qualifiedName ["Data", "Array"] "indexImpl", ForeignFunction indexImpl)
    , (qualifiedName ["Data", "Array"] "length", ForeignFunction len)
    , (qualifiedName ["Data", "Array"] "filter", ForeignFunction filterArray)
    , (qualifiedName ["Data", "HeytingAlgebra"] "boolConj", ForeignFunction (binOp (&&)))
    , (qualifiedName ["Data", "HeytingAlgebra"] "boolDisj", ForeignFunction (binOp (||)))
    , (qualifiedName ["Data", "HeytingAlgebra"] "boolNot", ForeignFunction ((pure :: a -> Eval a) . not))
    , (qualifiedName ["Data", "Eq"] "eqBooleanImpl", ForeignFunction (binOp ((==) @Bool)))
    , (qualifiedName ["Data", "Eq"] "eqIntImpl", ForeignFunction (binOp ((==) @Integer)))
    , (qualifiedName ["Data", "Eq"] "eqNumberImpl", ForeignFunction (binOp ((==) @Scientific)))
    , (qualifiedName ["Data", "Eq"] "eqCharImpl", ForeignFunction (binOp ((==) @Char)))
    , (qualifiedName ["Data", "Eq"] "eqStringImpl", ForeignFunction (binOp ((==) @Text)))
    , (qualifiedName ["Data", "Eq"] "eqArrayImpl", ForeignFunction eqArray)
    , (qualifiedName ["Data", "Ord"] "ordNumberImpl", ForeignFunction (ordImpl @Scientific))
    , (qualifiedName ["Data", "Foldable"] "foldlArray", ForeignFunction foldlArray)
    , (qualifiedName ["Data", "Foldable"] "foldrArray", ForeignFunction foldrArray)
    , (qualifiedName ["Data", "Semiring"] "intAdd", ForeignFunction (binOp ((+) @Integer)))
    , (qualifiedName ["Data", "Semiring"] "intMul", ForeignFunction (binOp ((*) @Integer)))
    , (qualifiedName ["Data", "Semiring"] "numAdd", ForeignFunction (binOp ((+) @Scientific)))
    , (qualifiedName ["Data", "Semiring"] "numMul", ForeignFunction (binOp ((*) @Scientific)))
    , (qualifiedName ["Data", "Ring"] "intSub", ForeignFunction (binOp ((-) @Integer)))
    , (qualifiedName ["Data", "Functor"] "arrayMap", ForeignFunction arrayMap)
    ]
  where
    indexImpl :: (Value EvalAnn -> Eval (Value EvalAnn)) -> Value EvalAnn -> Vector (Value EvalAnn) -> Integer -> Eval (Value EvalAnn)
    indexImpl just nothing xs i = maybe (pure nothing) just (xs ^? ix (fromIntegral i))
    
    len :: Vector (Value EvalAnn) -> Eval Integer
    len xs = pure (fromIntegral (Vector.length xs))

    filterArray :: (a ~ Value EvalAnn, b ~ Bool) => (a -> Eval b) -> Vector a -> Eval (Vector a)
    filterArray = Vector.filterM
  
    arrayMap :: (a ~ Value EvalAnn, b ~ Value EvalAnn) => (a -> Eval b) -> Vector a -> Eval (Vector b)
    arrayMap = Vector.mapM

    foldlArray :: (b ~ Value EvalAnn, a ~ Value EvalAnn) => (b -> a -> Eval b) -> b -> Vector a -> Eval b
    foldlArray = foldM

    foldrArray :: (b ~ Value EvalAnn, a ~ Value EvalAnn) => (a -> b -> Eval b) -> b -> Vector a -> Eval b
    foldrArray = foldrM

    binOp :: (a -> b -> c) -> a -> b -> Eval c
    binOp op x y = pure (x `op` y)

    eqArray :: (a ~ Value EvalAnn, b ~ Bool) => (a -> a -> Eval b) -> Vector a -> Vector a -> Eval b
    eqArray pred' v1 v2 
      | Vector.length v1 == Vector.length v2 = Vector.and <$> Vector.zipWithM pred' v1 v2
      | otherwise = pure False

    ordImpl :: forall a o. (Show a, Ord a, o ~ Value EvalAnn) => o -> o -> o -> a -> a -> Eval o
    ordImpl lt eq gt x y = pure $ case x `compare` y of
      LT -> lt
      EQ -> eq
      GT -> gt

evalForeignApply :: SourceSpan -> Env EvalAnn -> ApplyForeign -> Eval (Value EvalAnn)
evalForeignApply ss env (ApplyForeign qn paramNames) = do
  params <- for paramNames $ \n -> envLookupEval ss (Qualified Nothing n) env
  case Map.lookup qn foreignFunctions of
    (Just (ForeignFunction f)) -> evalForeignFunction ss env params f
    _ -> throwError (NotInScope ss qn)
