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
{-# LANGUAGE RankNTypes #-}
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
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Language.PureScript.AST (SourceSpan, displaySourceSpan, nullSourceSpan, spanName)
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString, mkString)
import Protolude hiding (moduleName, Meta)
import System.FilePath.Glob (glob)
import qualified WTP.Element as Element
import WTP.PureScript.Value
import System.FilePath ((</>))

data EvalError
  = UnexpectedError (Maybe SourceSpan) Text
  | UnexpectedType (Maybe SourceSpan) Text (Value EvalAnn)
  | EntryPointNotDefined (Qualified Ident)
  | NotInScope SourceSpan (Qualified Ident)
  | InvalidString SourceSpan
  | InvalidBuiltInFunctionApplication SourceSpan (Expr EvalAnn) (Expr EvalAnn)
  deriving (Show, Generic)

errorSourceSpan :: EvalError -> Maybe SourceSpan
errorSourceSpan = \case
  UnexpectedError ss _ -> ss
  UnexpectedType ss _ _ -> ss
  EntryPointNotDefined _ -> Nothing
  NotInScope ss _ -> Just ss
  InvalidString ss -> Just ss
  InvalidBuiltInFunctionApplication ss _ _ -> Just ss

instance Pretty EvalError where
  pretty = \case
    UnexpectedError _ t -> "Unexpected error:" <+> pretty t
    UnexpectedType _ t val -> "Expected value of type" <+> pretty t <+> "but got" <+> pretty val
    EntryPointNotDefined qn -> "Entry point not in scope:" <+> pretty (showQualified runIdent qn)
    NotInScope _ qn -> "Not in scope:" <+> pretty (showQualified runIdent qn)
    InvalidString _ -> "Invalid string"
    InvalidBuiltInFunctionApplication _ _fn _param -> "Invalid function application"

prettySourceSpan :: SourceSpan -> Doc ann
prettySourceSpan ss = pretty (displaySourceSpan mempty ss)

data EvalAnn = EvalAnn {annSourceSpan :: SourceSpan, annMeta :: Maybe Meta, annApplyForeign :: Maybe ApplyForeign}
  deriving (Show, Generic)

evalAnnFromAnn :: Ann -> EvalAnn
evalAnnFromAnn (ss, _, _, meta) = EvalAnn ss meta Nothing

type Eval a = Except EvalError a

runEval :: Eval a -> (Either EvalError a)
runEval = runExcept

unexpectedType :: (MonadError EvalError m) => SourceSpan -> Text -> Value EvalAnn -> m a
unexpectedType ss typ v =
  throwError
    ( UnexpectedType
        (Just ss)
        typ
        v
    )

sourceSpan :: Expr EvalAnn -> SourceSpan
sourceSpan = annSourceSpan . extractAnn

require ::
  forall (ctor :: Symbol) s t a b ann.
  (KnownSymbol ctor, AsConstructor ctor s t a b, ann ~ EvalAnn, s ~ Value ann, t ~ Value ann, a ~ b, Show ann) =>
  SourceSpan ->
  Proxy ctor ->
  Value ann ->
  Eval b
require ss (ctor :: Proxy ctor) v = case v ^? _Ctor @ctor of
  Just x -> pure x
  Nothing -> unexpectedType ss (Text.drop 1 (Text.pack (symbolVal ctor))) v

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
evalStringExpr expr = throwError (InvalidString (annSourceSpan (extractAnn expr)))

initialEnv :: Env EvalAnn
initialEnv =
  foldMap
    (\(qn, f) -> bindForeignFunction qn (arity f))
    (Map.toList foreignFunctions)
  where
    bindForeignFunction :: Qualified Ident -> Int -> Env EvalAnn
    bindForeignFunction qn arity' =
      envBindExpr qn (wrap arity' (\names -> Var (EvalAnn nullSourceSpan {spanName = toS (showQualified runIdent qn)} (Just IsForeign) (Just (ApplyForeign qn names))) qn))
    wrap :: Int -> ([Ident] -> Expr EvalAnn) -> Expr EvalAnn
    wrap arity' f =
      let names = [Ident ("x" <> show n) | n <- [1 .. arity']]
       in foldr (Abs (EvalAnn nullSourceSpan Nothing Nothing)) (f names) names

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
  Literal (EvalAnn ss _ _) lit -> case lit of
    NumericLiteral n -> pure (either VInt (VNumber . realToFrac) n)
    StringLiteral s -> VString <$> evalString ss s
    CharLiteral c -> pure (VChar c)
    BooleanLiteral b -> pure (VBool b)
    ArrayLiteral xs -> VArray . Vector.fromList <$> traverse (eval env) xs
    ObjectLiteral pairs -> do
      pairs' <- for pairs $ \(field', value) ->
        (,) <$> evalString ss field' <*> eval env value
      pure (VObject (HashMap.fromList pairs'))
  Constructor ann@(EvalAnn ss (Just IsNewtype) _) _ _ fieldNames -> do
    pure (VFunction (Function mempty (Ident "value") (Var (EvalAnn ss Nothing Nothing) (qualifiedName [] "value"))))
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
  Accessor (EvalAnn ss _ _) prop expr -> do
    key <- evalString ss prop
    obj <- require ss (Proxy @"VObject") =<< eval env expr
    maybe
      (throwError (UnexpectedError (Just ss) ("Key not present in object: " <> key)))
      pure
      (HashMap.lookup key obj)
  ObjectUpdate (EvalAnn ss _ _) expr updates -> do
    obj <- require ss (Proxy @"VObject") =<< eval env expr
    updates' <- for updates $ \(field', expr') ->
      (,) <$> evalString ss field' <*> eval env expr'
    pure (VObject (obj <> HashMap.fromList updates'))
  Abs ann arg body -> pure (VFunction (Function env arg body))
  App ann func param -> evalApp env ann func param
  Var _ (Qualified (Just (ModuleName [ProperName "Prim"])) (Ident "undefined")) -> pure (VObject mempty)
  Var (EvalAnn ss _ (Just applyForeign)) _ -> evalForeignApply ss env applyForeign
  Var (EvalAnn ss _ Nothing) qn -> envLookupEval ss qn env
  Case (EvalAnn ss _ _) exprs alts -> do
    values <- traverse (eval env) exprs
    evalCaseAlts ss env values alts
  Let (EvalAnn ss _ _) bindings body -> do
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
      evalFunc func' param'

evalFunc :: Function EvalAnn -> (Value EvalAnn) -> Eval (Value EvalAnn)
evalFunc (Function fEnv arg body) param' =
  let newEnv = (fEnv <> envBindValue (Qualified Nothing arg) param')
   in eval newEnv body

evalCaseAlts :: SourceSpan -> Env EvalAnn -> [(Value EvalAnn)] -> [CaseAlternative EvalAnn] -> Eval (Value EvalAnn)
evalCaseAlts ss _ vals [] = throwError (UnexpectedError (Just ss) (prettyText ("Non-exhaustive case expression on values:" <+> pretty vals)))
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
      (ConstructorBinder (EvalAnn _ (Just IsNewtype) _) _typeName (Qualified _ ctorName) [b], val) ->
        envFromBinder (b, val)
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
        JSON.Success (_, m) -> pure m { moduleDecls = map (addNameToDecl (moduleName m)) (moduleDecls m) }
        JSON.Error e -> throwError (toS e)
    Nothing -> throwError "Couldn't read CoreFn file."
  where
    addNameToDecl :: ModuleName -> Bind Ann -> Bind Ann
    addNameToDecl name = fmap (_1 . field @"spanName" .~ toS (runModuleName name))

loadAllModulesEnv :: [FilePath] -> ExceptT Text IO (Env EvalAnn)
loadAllModulesEnv paths = do
  ms <- traverse loadModule paths
  pure (foldMap toModuleEnv ms)

evalEntryPoint :: Qualified Ident -> Env EvalAnn -> Eval (Value EvalAnn)
evalEntryPoint entryPoint env =
  envLookupEval nullSourceSpan entryPoint env

runWithEntryPoint :: forall a. ToHaskellValue a => Qualified Ident -> IO (Either Text a)
runWithEntryPoint entry = runExceptT $ do
  let coreFnPath :: Text -> FilePath
      coreFnPath mn = "output" </> toS mn </> "corefn.json"
  stdPaths <- liftIO (glob (coreFnPath "*") <&> filter (/= coreFnPath "Effect"))
  specPath <- case asQualifiedName entry of
    Just (mns, _) -> pure (coreFnPath (Text.intercalate "." mns))
    Nothing -> throwError ("Unqualified entry point")
  let paths = stdPaths <> pure specPath
  liftIO (putStrLn ("Loading " <> show (length paths) <> " modules..." :: Text))
  env <- loadAllModulesEnv paths
  case runEval (evalEntryPoint entry (initialEnv <> env) >>= toHaskellValue nullSourceSpan) of
    Right value -> pure value
    Left err ->
      let prefix = case errorSourceSpan err of
            Just ss -> prettySourceSpan ss <> ":" <> line <> "error:"
            Nothing -> "<no source information>" <> "error:"
        in throwError (prettyText (prefix <+> pretty err))

test :: Qualified Ident -> IO ()
test ep = runWithEntryPoint @Bool ep >>= \case
  Left err -> putStrLn err >> exitWith (ExitFailure 1)
  Right successful -> putStrLn (prettyText ("Result:" <+> pretty successful))

prettyText :: Doc ann -> Text
prettyText x = renderStrict (layoutPretty defaultLayoutOptions x)

-- * Foreign Functions

data ApplyForeign = ApplyForeign (Qualified Ident) [Ident]
  deriving (Show, Generic)

data ForeignFunction
  = ForeignFunction
      { arity :: Int,
        evalFn :: (SourceSpan -> Env EvalAnn -> [Value EvalAnn] -> Eval (Value EvalAnn))
      }

foreignFunction :: (EvalForeignFunction f, ForeignFunctionArity f) => f -> ForeignFunction
foreignFunction (f :: f) =
  ForeignFunction
    (foreignFunctionArity (Proxy :: Proxy f))
    (\ss env params -> evalForeignFunction ss env params f)

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

instance {-# OVERLAPPABLE #-} FromHaskellValue a => EvalForeignFunction (Eval a) where
  evalForeignFunction _ _ [] x = fromHaskellValue <$> x
  evalForeignFunction ss _ _ _ = foreignFunctionArityMismatch ss

instance {-# OVERLAPPING #-} (ToHaskellValue a, EvalForeignFunction b) => EvalForeignFunction (a -> b) where
  evalForeignFunction ss env (v : vs) f = do
    a <- toHaskellValue ss v
    evalForeignFunction ss env vs (f a)
  evalForeignFunction ss _ [] _ = foreignFunctionArityMismatch ss

class ToHaskellValue r where
  toHaskellValue :: SourceSpan -> Value EvalAnn -> Eval r

instance ToHaskellValue (Value EvalAnn) where
  toHaskellValue _ = pure

instance ToHaskellValue Bool where
  toHaskellValue ss = require ss (Proxy @"VBool")

instance ToHaskellValue Text where
  toHaskellValue ss = require ss (Proxy @"VString")

instance ToHaskellValue Char where
  toHaskellValue ss = require ss (Proxy @"VChar")

instance ToHaskellValue Integer where
  toHaskellValue ss = require ss (Proxy @"VInt")

instance ToHaskellValue Scientific where
  toHaskellValue ss = require ss (Proxy @"VNumber")

instance ToHaskellValue a => ToHaskellValue (Vector a) where
  toHaskellValue ss = traverse (toHaskellValue ss) <=< require ss (Proxy @"VArray")

instance (FromHaskellValue a, ToHaskellValue b) => ToHaskellValue (a -> Eval b) where
  toHaskellValue ss fn =
    pure
      ( \x -> do
          fn' <- require ss (Proxy @"VFunction") fn
          b <- evalFunc fn' (fromHaskellValue x)
          toHaskellValue ss b
      )

instance (Show a, Show b, FromHaskellValue a, FromHaskellValue b, ToHaskellValue c) => ToHaskellValue (a -> b -> Eval c) where
  toHaskellValue ss fn = do
    pure
      ( \a b -> do
          fn' <- require ss (Proxy @"VFunction") fn
          fn'' <- require ss (Proxy @"VFunction") =<< evalFunc fn' (fromHaskellValue a)
          c <- evalFunc fn'' (fromHaskellValue b)
          toHaskellValue ss c
      )

class FromHaskellValue a where
  fromHaskellValue :: a -> Value EvalAnn

instance FromHaskellValue Bool where
  fromHaskellValue = VBool

instance FromHaskellValue Integer where
  fromHaskellValue = VInt

instance FromHaskellValue Scientific where
  fromHaskellValue = VNumber

instance FromHaskellValue Char where
  fromHaskellValue = VChar

instance FromHaskellValue Text where
  fromHaskellValue = VString

instance FromHaskellValue a => FromHaskellValue (Vector a) where
  fromHaskellValue xs = VArray (fromHaskellValue <$> xs)

instance FromHaskellValue (Value EvalAnn) where
  fromHaskellValue = identity

foreignFunctions :: Map (Qualified Ident) ForeignFunction
foreignFunctions =
  Map.fromList
    [ (qualifiedName ["Control", "Bind"] "arrayBind", foreignFunction arrayBind),
      (qualifiedName ["Data", "Array"] "indexImpl", foreignFunction indexImpl),
      (qualifiedName ["Data", "Array"] "length", foreignFunction len),
      (qualifiedName ["Data", "Array"] "filter", foreignFunction filterArray),
      (qualifiedName ["Data", "HeytingAlgebra"] "boolConj", foreignFunction (binOp (&&))),
      (qualifiedName ["Data", "HeytingAlgebra"] "boolDisj", foreignFunction (binOp (||))),
      (qualifiedName ["Data", "HeytingAlgebra"] "boolNot", foreignFunction ((pure :: a -> Eval a) . not)),
      (qualifiedName ["Data", "Eq"] "eqBooleanImpl", foreignFunction (binOp ((==) @Bool))),
      (qualifiedName ["Data", "Eq"] "eqIntImpl", foreignFunction (binOp ((==) @Integer))),
      (qualifiedName ["Data", "Eq"] "eqNumberImpl", foreignFunction (binOp ((==) @Scientific))),
      (qualifiedName ["Data", "Eq"] "eqCharImpl", foreignFunction (binOp ((==) @Char))),
      (qualifiedName ["Data", "Eq"] "eqStringImpl", foreignFunction (binOp ((==) @Text))),
      (qualifiedName ["Data", "Eq"] "eqArrayImpl", foreignFunction eqArray),
      (qualifiedName ["Data", "Ord"] "ordNumberImpl", foreignFunction (ordImpl @Scientific)),
      (qualifiedName ["Data", "Foldable"] "foldlArray", foreignFunction foldlArray),
      (qualifiedName ["Data", "Foldable"] "foldrArray", foreignFunction foldrArray),
      (qualifiedName ["Data", "Semiring"] "intAdd", foreignFunction (binOp ((+) @Integer))),
      (qualifiedName ["Data", "Semiring"] "intMul", foreignFunction (binOp ((*) @Integer))),
      (qualifiedName ["Data", "Semiring"] "numAdd", foreignFunction (binOp ((+) @Scientific))),
      (qualifiedName ["Data", "Semiring"] "numMul", foreignFunction (binOp ((*) @Scientific))),
      (qualifiedName ["Data", "Semigroup"] "concatString", foreignFunction (binOp ((<>) @Text))),
      (qualifiedName ["Data", "Ring"] "intSub", foreignFunction (binOp ((-) @Integer))),
      (qualifiedName ["Data", "Functor"] "arrayMap", foreignFunction arrayMap)
    ]
  where
    indexImpl :: a ~ (Value EvalAnn) => (a -> Eval (Value EvalAnn)) -> Value EvalAnn -> Vector a -> Integer -> Eval (Value EvalAnn)
    indexImpl just nothing xs i = maybe (pure nothing) just (xs ^? ix (fromIntegral i))
    len :: Vector (Value EvalAnn) -> Eval Integer
    len xs = pure (fromIntegral (Vector.length xs))
    filterArray :: (a ~ Value EvalAnn, b ~ Bool) => (a -> Eval b) -> Vector a -> Eval (Vector a)
    filterArray = Vector.filterM
    arrayMap :: (a ~ Value EvalAnn, b ~ Value EvalAnn) => (a -> Eval b) -> Vector a -> Eval (Vector b)
    arrayMap = Vector.mapM
    arrayBind :: (a ~ Value EvalAnn, b ~ Value EvalAnn) => Vector a -> (a -> Eval (Vector b)) -> Eval (Vector b)
    arrayBind xs f = join <$> traverse f xs
    foldlArray :: (b ~ Value EvalAnn, a ~ Value EvalAnn) => (b -> a -> Eval b) -> b -> Vector a -> Eval b
    foldlArray = foldM
    foldrArray :: (b ~ Value EvalAnn, a ~ Value EvalAnn) => (a -> b -> Eval b) -> b -> Vector a -> Eval b
    foldrArray = foldrM
    binOp :: Show a => (a -> a -> b) -> a -> a -> Eval b
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
    (Just f) -> evalFn f ss env params
    _ -> throwError (NotInScope ss qn)
