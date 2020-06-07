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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WebCheck.PureScript where

import Control.Lens hiding (op)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Fixed (mod')
import Data.Generics.Product (field)
import Data.Generics.Sum (AsConstructor, _Ctor)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Text.Read as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Language.PureScript as P
import Language.PureScript.AST (SourceSpan, internalModuleSourceSpan, nullSourceSpan, sourcePosColumn, sourcePosLine, spanEnd, spanName, spanStart)
import qualified Language.PureScript.CST as CST
import Language.PureScript.CoreFn
import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString, mkString)
import Protolude hiding (Meta, moduleName)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import Text.Read (read)
import qualified WebCheck.Element as WebCheck
import WebCheck.PureScript.Value
import qualified WebCheck.Specification as WebCheck

data EvalError
  = UnexpectedError (Maybe SourceSpan) Text
  | UnexpectedType (Maybe SourceSpan) Text (Value EvalAnn)
  | EntryPointNotDefined (Qualified Ident)
  | NotInScope SourceSpan (Qualified Ident)
  | ForeignFunctionNotSupported SourceSpan (Qualified Ident)
  | InvalidString SourceSpan
  | InvalidBuiltInFunctionApplication SourceSpan (Expr EvalAnn) (Expr EvalAnn)
  | ForeignFunctionError (Maybe SourceSpan) Text
  | Undetermined
  deriving (Show, Generic)

errorSourceSpan :: EvalError -> Maybe SourceSpan
errorSourceSpan = \case
  UnexpectedError ss _ -> ss
  UnexpectedType ss _ _ -> ss
  EntryPointNotDefined _ -> Nothing
  NotInScope ss _ -> Just ss
  ForeignFunctionNotSupported ss _ -> Just ss
  InvalidString ss -> Just ss
  InvalidBuiltInFunctionApplication ss _ _ -> Just ss
  ForeignFunctionError ss _ -> ss
  Undetermined -> Nothing

instance Pretty EvalError where
  pretty = \case
    UnexpectedError _ t -> "Unexpected error:" <+> pretty t
    UnexpectedType _ t val -> "Expected value of type" <+> pretty t <+> "but got" <+> pretty val
    EntryPointNotDefined qn -> "Entry point not in scope:" <+> pretty (showQualified runIdent qn)
    NotInScope _ qn -> "Not in scope:" <+> pretty (showQualified runIdent qn)
    ForeignFunctionNotSupported _ qn -> "Foreign function is not supported in WebCheck:" <+> pretty (showQualified runIdent qn)
    InvalidString _ -> "Invalid string"
    InvalidBuiltInFunctionApplication _ _fn _param -> "Invalid function application"
    ForeignFunctionError _ t -> pretty t
    Undetermined -> "The formula cannot be determined as there are not enough observed states"

prettySourceSpan :: SourceSpan -> Doc ann
prettySourceSpan ss =
  pretty (spanName ss)
    <> colon
    <> pretty (sourcePosLine (spanStart ss))
    <> colon
    <> pretty (sourcePosColumn (spanStart ss))
    <> "-"
    <> pretty (sourcePosLine (spanEnd ss))
    <> colon
    <> pretty (sourcePosColumn (spanEnd ss))

data EvalAnn = EvalAnn {annSourceSpan :: SourceSpan, annMeta :: Maybe Meta, annApplyForeign :: Maybe ApplyForeign}
  deriving (Show, Generic)

evalAnnFromAnn :: Ann -> EvalAnn
evalAnnFromAnn (ss, _, _, meta) = EvalAnn ss meta Nothing

newtype Eval a = Eval (ExceptT EvalError (Reader [(Int, QueriedElements)]) a)
  deriving (Functor, Applicative, Monad, MonadError EvalError, MonadFix, MonadReader [(Int, QueriedElements)])

runEval :: [QueriedElements] -> Eval a -> (Either EvalError a)
runEval observedStates (Eval ma) = runReader (runExceptT ma) (zip [1 ..] observedStates)

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
  (KnownSymbol ctor, AsConstructor ctor s t a b, ann ~ EvalAnn, s ~ Value ann, t ~ Value ann, a ~ b) =>
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
    builtInSS = internalModuleSourceSpan "<builtin>"
    bindForeignFunction :: Qualified Ident -> Int -> Env EvalAnn
    bindForeignFunction qn arity' =
      envBindExpr qn (wrap arity' (\names -> Var (EvalAnn builtInSS {spanName = toS (showQualified runIdent qn)} (Just IsForeign) (Just (ApplyForeign qn names))) qn))
    wrap :: Int -> ([Ident] -> Expr EvalAnn) -> Expr EvalAnn
    wrap arity' f =
      let names = [Ident ("x" <> show n) | n <- [1 .. arity']]
       in foldr (Abs (EvalAnn builtInSS Nothing Nothing)) (f names) names

envLookupEval :: SourceSpan -> Qualified Ident -> Env EvalAnn -> Eval (Value EvalAnn)
envLookupEval ss qn env' =
  case envLookup qn env' of
    Just r -> either (eval (withoutLocals env')) onValue r
    Nothing -> throwError (NotInScope ss qn)
  where
    onValue (VDefer (Defer env'' expr')) = eval env'' expr'
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

accessField :: SourceSpan -> Text -> HashMap Text (Value EvalAnn) -> Eval (Value EvalAnn)
accessField ss key obj =
  maybe
    (throwError (UnexpectedError (Just ss) ("Key not present in object: " <> key)))
    pure
    (HashMap.lookup key obj)

type QueriedElements = HashMap WebCheck.Selector [HashMap WebCheck.ElementState (Value EvalAnn)]

pattern BuiltIn :: Text -> a -> Expr a -> Expr a
pattern BuiltIn name ann p <- App ann (Var _ (Qualified (Just (ModuleName "WebCheck.DSL")) (Ident name))) p

pattern Always :: a -> Expr a -> Expr a
pattern Always ann p <- BuiltIn "always" ann p

pattern Next :: a -> Expr a -> Expr a
pattern Next ann p <- BuiltIn "next" ann p

eval :: Env EvalAnn -> Expr EvalAnn -> Eval (Value EvalAnn)
eval env expr =
  ask >>= \case
    [] -> case expr of
      Always _ _ -> pure (VBool True)
      _ -> throwError Undetermined
    ((n, current) : rest) -> case expr of
      -- Special cases
      Next _ p -> local (const rest) (eval env p)
      Always ann p -> do
        first' <- require (sourceSpan p) (Proxy @"VBool")
          =<< eval env p `catchError` \case
            Undetermined -> pure (VBool True)
            e -> throwError e
        rest' <-
          require (annSourceSpan ann) (Proxy @"VBool")
            =<< local (drop 1) (eval env expr)
        pure (VBool (first' && rest'))
      App _ (BuiltIn "trace" _ label) p -> do
        t <- require (sourceSpan label) (Proxy @"VString") =<< eval env label
        traceM
          ( prettyText
              ( prettySourceSpan (sourceSpan expr) <> colon
                  <+> "trace: in state"
                  <+> pretty n <> colon
                  <+> pretty t
              )
          )
        eval env p
      App _ (BuiltIn "_queryAll" _ p) q -> evalQuery env p q current
      BuiltIn "_property" _ p -> do
        name <- require (sourceSpan p) (Proxy @"VString") =<< eval env p
        pure (VElementState (WebCheck.Property name))
      BuiltIn "_attribute" _ p -> do
        name <- require (sourceSpan p) (Proxy @"VString") =<< eval env p
        pure (VElementState (WebCheck.Attribute name))
      -- General cases
      Literal (EvalAnn ss _ _) lit -> case lit of
        NumericLiteral n' -> pure (either (VInt . fromInteger) (VNumber . realToFrac) n')
        StringLiteral s -> VString <$> evalString ss s
        CharLiteral c -> pure (VChar c)
        BooleanLiteral b -> pure (VBool b)
        ArrayLiteral xs -> VArray . Vector.fromList <$> traverse (eval env) xs
        ObjectLiteral pairs -> do
          pairs' <- for pairs $ \(field', value) ->
            (,) <$> evalString ss field' <*> eval env value
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
        eval env (foldr (Abs ann) body fieldNames)
      Accessor (EvalAnn ss _ _) prop objExpr -> do
        key <- evalString ss prop
        obj <- require ss (Proxy @"VObject") =<< eval env objExpr
        accessField ss key obj
      ObjectUpdate (EvalAnn ss _ _) objExpr updates -> do
        obj <- require ss (Proxy @"VObject") =<< eval env objExpr
        updates' <- for updates $ \(field', expr') ->
          (,) <$> evalString ss field' <*> eval env expr'
        pure (VObject (obj <> HashMap.fromList updates'))
      Abs _ann arg body -> pure (VFunction (Function env arg body))
      App _ func param -> do
        func' <- require (sourceSpan func) (Proxy @"VFunction") =<< eval env func
        param' <- eval env param
        evalFunc func' param'
      Var _ (Qualified (Just (ModuleName "Prim")) (Ident "undefined")) -> pure (VObject mempty)
      Var (EvalAnn ss _ (Just applyForeign)) _ -> evalForeignApply ss env applyForeign
      Var (EvalAnn ss _ Nothing) qn -> envLookupEval ss qn env
      Case (EvalAnn ss _ _) exprs alts -> do
        values <- traverse (eval env) exprs
        evalCaseAlts ss env values alts
      Let (EvalAnn _ss _ _) bindings body -> do
        let bindingEnv env' = \case
              NonRec _ name expr' -> do
                pure (env' <> envBindValue (Qualified Nothing name) (VDefer (Defer env' expr')))
              Rec binds -> do
                rec recEnv <-
                      fold
                        <$> traverse
                          ( \((_, name), expr') ->
                              envBindValue (Qualified Nothing name) <$> pure (VDefer (Defer (env' <> recEnv) expr'))
                          )
                          binds
                pure recEnv
        newEnv <- foldM bindingEnv env bindings
        eval (env <> newEnv) body

evalQuery :: Env EvalAnn -> Expr EvalAnn -> Expr EvalAnn -> QueriedElements -> Eval (Value EvalAnn)
evalQuery env p1 p2 current = do
  selector <- require (sourceSpan p1) (Proxy @"VString") =<< eval env p1
  wantedStates <- require (sourceSpan p2) (Proxy @"VObject") =<< eval env p2
  matchedElements <-
    maybe
      (throwError (ForeignFunctionError (Just (sourceSpan p1)) ("Selector not in observed state: " <> selector)))
      pure
      (HashMap.lookup (WebCheck.Selector selector) current)
  mappedElements <- for (Vector.fromList matchedElements) $ \matchedElement -> do
    mappings <- flip HashMap.traverseWithKey wantedStates $ \k s -> do
      elementState <- require (sourceSpan p2) (Proxy @"VElementState") s
      case HashMap.lookup elementState matchedElement of
        Just x ->
          pure x
        Nothing ->
          let msg = ("Element state (bound to ." <> k <> ") not in observed state for query `" <> selector <> "`: " <> show elementState)
           in throwError (ForeignFunctionError (Just (sourceSpan p2)) msg)
    pure (VObject mappings)
  pure (VArray mappedElements)

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

toModuleEnv :: Module Ann -> Env EvalAnn
toModuleEnv m =
  let addDecl = \case
        NonRec _ name expr -> bindExpr name expr
        Rec binds -> foldMap (\((_, name), expr) -> bindExpr name expr) binds
   in foldMap addDecl (moduleDecls m)
  where
    bindExpr name expr = envBindExpr (Qualified (Just (moduleName m)) name) (evalAnnFromAnn <$> expr)

loadModuleFromSource :: Modules -> Text -> ExceptT Text IO (Module Ann)
loadModuleFromSource modules input =
    case CST.parseModuleFromFile "<file>" input >>= CST.resFull of
      Left parseError ->
        -- _ $ CST.toMultipleErrors "<file>" parseError
        throwError (show parseError)
      Right m -> do
        (result, _) <- withExceptT show . runWriterT . flip runReaderT P.defaultOptions $ do
          (P.Module ss coms moduleName elaborated exps, env) <- fmap fst . P.runSupplyT 0 $ do
            desugared <- P.desugar (modulesNamesEnv modules) (modulesExterns modules) [P.importPrim m] >>= \case
              [d] -> pure d
              _ -> throwError (P.MultipleErrors mempty)
            P.runCheck' (P.emptyCheckState (modulesInitEnv modules)) $ P.typeCheckModule desugared
          regrouped <- P.createBindingGroups moduleName . P.collapseBindingGroups $ elaborated
          let mod'' = P.Module ss coms moduleName regrouped exps
          pure (CF.moduleToCoreFn env mod'')
        pure result

loadModuleFromCoreFn :: FilePath -> ExceptT Text IO (Module Ann)
loadModuleFromCoreFn path = do
  j <- liftIO (BS.readFile path)
  case JSON.decode j of
    Just val ->
      case JSON.parse moduleFromJSON val of
        JSON.Success (_, m) -> do
          putStrLn ("Loaded " <> runModuleName (moduleName m))
          pure m {moduleDecls = map (addNameToDecl (toS (modulePath m))) (moduleDecls m)}
        JSON.Error e -> throwError (toS e)
    Nothing -> throwError "Couldn't read CoreFn file."
  where
    addNameToDecl :: Text -> Bind Ann -> Bind Ann
    addNameToDecl name = fmap (_1 . field @"spanName" .~ toS name)

data Modules
  = Modules
      { modulesCoreFn :: [Module Ann],
        modulesExterns :: [P.ExternsFile],
        modulesNamesEnv :: P.Env,
        modulesInitEnv :: P.Environment
      }
  deriving (Show)

loadModulesFromCoreFn :: FilePath -> ExceptT Text IO [Module Ann]
loadModulesFromCoreFn webcheckPursDir = do
  let coreFnPath :: Text -> FilePath
      coreFnPath mn' = webcheckPursDir </> toS mn' </> "corefn.json"
  paths <- liftIO (glob (coreFnPath "*"))
  traverse loadModuleFromCoreFn paths

loadExterns :: ModuleName -> FilePath -> ExceptT Text IO P.ExternsFile
loadExterns (ModuleName mn) webcheckPursDir = do
  let path = webcheckPursDir </> toS mn </> "externs.cbor"
  withExceptT show (P.readExternsFile path) >>= \case
    Just ext -> pure ext
    Nothing -> throwError ("Could not read externs file: " <> toS path)

loadLibraryModules :: FilePath -> IO (Either Text Modules)
loadLibraryModules webcheckPursDir = runExceptT $ do
  libModules <- loadModulesFromCoreFn webcheckPursDir
  externs <- for libModules $ \m -> loadExterns (moduleName m) webcheckPursDir
  sortedExterns <- withExceptT show . fmap fst $ P.sortModules externModuleSignature externs
  namesEnv <- withExceptT show . fmap fst . runWriterT $ foldM P.externsEnv P.primEnv sortedExterns
  let initEnv = foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment sortedExterns
  pure (Modules libModules sortedExterns namesEnv initEnv)
  where
    externModuleSignature e =
      P.ModuleSignature
        (P.efSourceSpan e)
        (P.efModuleName e)
        (map ((,nullSourceSpan) . P.eiModule) (P.efImports e))

data Program
  = Program
      { programLibraryModules :: Modules,
        programMain :: Module Ann,
        programEnv :: Env EvalAnn
      }
  deriving (Show)

loadProgram :: Modules -> Text -> IO (Either Text Program)
loadProgram ms input = runExceptT $ do
  specModule <- loadModuleFromSource ms input
  let env' = foldMap toModuleEnv (modulesCoreFn ms <> [specModule])
  pure
    ( Program
        { programLibraryModules = ms,
          programMain = specModule,
          programEnv = initialEnv <> env'
        }
    )

entrySS :: SourceSpan
entrySS = internalModuleSourceSpan "<entry>"

evalEntryPoint :: Qualified Ident -> Program -> Eval (Value EvalAnn)
evalEntryPoint entryPoint prog = envLookupEval entrySS entryPoint (programEnv prog)

runWithEntryPoint :: forall a. ToHaskellValue a => [QueriedElements] -> Qualified Ident -> Program -> IO (Either Text a)
runWithEntryPoint observedStates entry prog = runExceptT $ do
  case runEval observedStates (evalEntryPoint entry prog >>= toHaskellValue entrySS) of
    Right value -> pure value
    Left err ->
      let prefix = case errorSourceSpan err of
            Just ss -> prettySourceSpan ss <> ":" <> line <> "error:"
            Nothing -> "<no source information>" <> "error:"
       in throwError (prettyText (prefix <+> pretty err))

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
foreignFunctionArityMismatch ss = throwError (ForeignFunctionError (Just ss) "Foreign function arity mismatch")

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

instance ToHaskellValue Int where
  toHaskellValue ss = require ss (Proxy @"VInt")

instance ToHaskellValue Double where
  toHaskellValue ss = require ss (Proxy @"VNumber")

instance ToHaskellValue a => ToHaskellValue (Vector a) where
  toHaskellValue ss = traverse (toHaskellValue ss) <=< require ss (Proxy @"VArray")

instance ToHaskellValue a => ToHaskellValue [a] where
  toHaskellValue ss x = Vector.toList <$> toHaskellValue ss x

instance ToHaskellValue a => ToHaskellValue (HashMap Text a) where
  toHaskellValue ss = traverse (toHaskellValue ss) <=< require ss (Proxy @"VObject")

instance ToHaskellValue (WebCheck.Action WebCheck.Selector) where
  toHaskellValue ss v = do
    obj <- require ss (Proxy @"VObject") v
    ctor <- require ss (Proxy @"VString") =<< accessField ss "constructor" obj
    value <- Vector.head <$> (require ss (Proxy @"VArray") =<< accessField ss "fields" obj)
    case ctor of
      "Focus" -> WebCheck.Focus . WebCheck.Selector <$> toHaskellValue ss value
      "KeyPress" -> WebCheck.KeyPress <$> toHaskellValue ss value
      "Click" -> WebCheck.Click . WebCheck.Selector <$> toHaskellValue ss value
      "Navigate" -> WebCheck.Navigate . WebCheck.Path <$> toHaskellValue ss value
      _ -> throwError (ForeignFunctionError (Just ss) ("Unknown Action constructor: " <> ctor))

instance (FromHaskellValue a, ToHaskellValue b) => ToHaskellValue (a -> Eval b) where
  toHaskellValue ss fn =
    pure
      ( \x -> do
          fn' <- require ss (Proxy @"VFunction") fn
          b <- evalFunc fn' (fromHaskellValue x)
          toHaskellValue ss b
      )

instance (FromHaskellValue a, FromHaskellValue b, ToHaskellValue c) => ToHaskellValue (a -> b -> Eval c) where
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

instance FromHaskellValue Int where
  fromHaskellValue = VInt

instance FromHaskellValue Double where
  fromHaskellValue = VNumber

instance FromHaskellValue Char where
  fromHaskellValue = VChar

instance FromHaskellValue Text where
  fromHaskellValue = VString

instance FromHaskellValue a => FromHaskellValue (Vector a) where
  fromHaskellValue xs = VArray (fromHaskellValue <$> xs)

instance FromHaskellValue a => FromHaskellValue [a] where
  fromHaskellValue = fromHaskellValue . Vector.fromList

instance FromHaskellValue (Value EvalAnn) where
  fromHaskellValue = identity

foreignFunctions :: Map (Qualified Ident) ForeignFunction
foreignFunctions =
  Map.fromList
    [ (qualifiedName "Control.Bind" "arrayBind", foreignFunction arrayBind),
      (qualifiedName "Data.Array" "indexImpl", foreignFunction indexImpl),
      (qualifiedName "Data.Array" "length", foreignFunction len),
      (qualifiedName "Data.Array" "filter", foreignFunction filterArray),
      (qualifiedName "Data.Bounded" "bottomInt", foreignFunction (pure minBound :: Eval Int)),
      (qualifiedName "Data.Bounded" "topInt", foreignFunction (pure maxBound :: Eval Int)),
      (qualifiedName "Data.Eq" "eqBooleanImpl", foreignFunction (op2 ((==) @Bool))),
      (qualifiedName "Data.Eq" "eqIntImpl", foreignFunction (op2 ((==) @Int))),
      (qualifiedName "Data.Eq" "eqNumberImpl", foreignFunction (op2 ((==) @Double))),
      (qualifiedName "Data.Eq" "eqCharImpl", foreignFunction (op2 ((==) @Char))),
      (qualifiedName "Data.Eq" "eqStringImpl", foreignFunction (op2 ((==) @Text))),
      (qualifiedName "Data.Eq" "eqArrayImpl", foreignFunction eqArray),
      (qualifiedName "Data.EuclideanRing" "intDegree", foreignFunction intDegree),
      (qualifiedName "Data.EuclideanRing" "intDiv", foreignFunction intDiv),
      (qualifiedName "Data.EuclideanRing" "intMod", foreignFunction intMod),
      (qualifiedName "Data.EuclideanRing" "numDiv", foreignFunction (op2 @Double (/))),
      (qualifiedName "Data.Foldable" "foldlArray", foreignFunction foldlArray),
      (qualifiedName "Data.Foldable" "foldrArray", foreignFunction foldrArray),
      (qualifiedName "Data.Functor" "arrayMap", foreignFunction arrayMap),
      (qualifiedName "Data.HeytingAlgebra" "boolConj", foreignFunction (op2 (&&))),
      (qualifiedName "Data.HeytingAlgebra" "boolDisj", foreignFunction (op2 (||))),
      (qualifiedName "Data.HeytingAlgebra" "boolNot", foreignFunction ((pure :: a -> Eval a) . not)),
      (qualifiedName "Data.Int" "toNumber", foreignFunction ((pure :: Double -> Eval Double) . fromIntegral @Int)),
      (qualifiedName "Data.Int" "fromNumberImpl", foreignFunction fromNumberImpl),
      (qualifiedName "Data.Int" "fromStringAsImpl", foreignFunction fromStringAsImpl),
      (qualifiedName "Data.Ord" "ordBooleanImpl", foreignFunction (ordImpl @Bool)),
      (qualifiedName "Data.Ord" "ordIntImpl", foreignFunction (ordImpl @Int)),
      (qualifiedName "Data.Ord" "ordNumberImpl", foreignFunction (ordImpl @Double)),
      (qualifiedName "Data.Ord" "ordStringImpl", foreignFunction (ordImpl @Text)),
      (qualifiedName "Data.Ord" "ordCharImpl", foreignFunction (ordImpl @Char)),
      (qualifiedName "Data.Ring" "intSub", foreignFunction (op2 ((-) @Int))),
      (qualifiedName "Data.Ring" "numSub", foreignFunction (op2 ((-) @Double))),
      (qualifiedName "Data.Show" "showStringImpl", foreignFunction (op1 (show @Text @Text))),
      (qualifiedName "Data.Show" "showIntImpl", foreignFunction (op1 (show @Int @Text))),
      (qualifiedName "Data.Show" "showNumberImpl", foreignFunction (op1 (show @Double @Text))),
      (qualifiedName "Data.Show" "cons", foreignFunction (op2 (Vector.cons @(Value EvalAnn)))),
      (qualifiedName "Data.Show" "join", foreignFunction (op2 Text.intercalate)),
      (qualifiedName "Data.Semiring" "intAdd", foreignFunction (op2 ((+) @Int))),
      (qualifiedName "Data.Semiring" "intMul", foreignFunction (op2 ((*) @Int))),
      (qualifiedName "Data.Semiring" "numAdd", foreignFunction (op2 ((+) @Double))),
      (qualifiedName "Data.Semiring" "numMul", foreignFunction (op2 ((*) @Double))),
      (qualifiedName "Data.Semigroup" "concatString", foreignFunction (op2 ((<>) @Text))),
      (qualifiedName "Data.Semigroup" "concatArray", foreignFunction (op2 ((<>) @(Vector (Value EvalAnn))))),
      notSupported (qualifiedName "Data.String.Common" "_localeCompare"),
      (qualifiedName "Data.String.Common" "replace", foreignFunction (op3 Text.replace)),
      (qualifiedName "Data.String.Common" "split", foreignFunction (op2 Text.splitOn)),
      (qualifiedName "Data.String.Common" "toLower", foreignFunction (op1 Text.toLower)),
      (qualifiedName "Data.String.Common" "toUpper", foreignFunction (op1 Text.toUpper)),
      (qualifiedName "Data.String.Common" "trim", foreignFunction (op1 Text.strip)),
      (qualifiedName "Data.String.Common" "joinWith", foreignFunction (op2 Text.intercalate)),
      (qualifiedName "Data.Unfoldable" "unfoldrArrayImpl", foreignFunction unfoldrArrayImpl),
      (qualifiedName "Global" "infinity", foreignFunction (pure (read "Infinity" :: Double) :: Eval Double)),
      (qualifiedName "Global" "nan", foreignFunction (pure (read "NaN" :: Double) :: Eval Double)),
      (qualifiedName "Global" "isFinite", foreignFunction (op1 (not . isInfinite @Double))),
      (qualifiedName "Global" "readFloat", foreignFunction (readAs Text.double)),
      (qualifiedName "Global" "readInt", foreignFunction readInt),
      (qualifiedName "Math" "floor", foreignFunction (op1 (fromIntegral @Int @Double . floor @Double @Int))),
      (qualifiedName "Math" "remainder", foreignFunction (op2 (mod' @Double))),
      (qualifiedName "Partial.Unsafe" "unsafePartial", foreignFunction unsafePartial),
      (qualifiedName "Record.Unsafe" "unsafeGet", foreignFunction (accessField nullSourceSpan))
    ]
  where
    notSupported :: Qualified Ident -> (Qualified Ident, ForeignFunction)
    notSupported qn = (qn, ForeignFunction 0 (\ss _env _params -> throwError (ForeignFunctionNotSupported ss qn)))
    indexImpl :: a ~ (Value EvalAnn) => (a -> Eval (Value EvalAnn)) -> Value EvalAnn -> Vector a -> Int -> Eval (Value EvalAnn)
    indexImpl just nothing xs i = maybe (pure nothing) just (xs ^? ix (fromIntegral i))
    fromNumberImpl :: (Int -> Eval (Value EvalAnn)) -> Value EvalAnn -> Double -> Eval (Value EvalAnn)
    fromNumberImpl just _ = just . round
    fromStringAsImpl :: (Int -> Eval (Value EvalAnn)) -> Value EvalAnn -> Int -> Text -> Eval (Value EvalAnn)
    fromStringAsImpl just nothing radix t =
      either (const (pure nothing)) (just . fst) $ case radix of
        10 -> Text.decimal t
        16 -> Text.hexadecimal t
        _ -> Left mempty
    len :: Vector (Value EvalAnn) -> Eval Int
    len xs = pure (fromIntegral (Vector.length xs))
    filterArray :: (Value EvalAnn -> Eval Bool) -> Vector (Value EvalAnn) -> Eval (Vector (Value EvalAnn))
    filterArray = Vector.filterM
    arrayMap :: (a ~ Value EvalAnn, b ~ Value EvalAnn) => (a -> Eval b) -> Vector a -> Eval (Vector b)
    arrayMap = Vector.mapM
    arrayBind :: (a ~ Value EvalAnn, b ~ Value EvalAnn) => Vector a -> (a -> Eval (Vector b)) -> Eval (Vector b)
    arrayBind xs f = join <$> traverse f xs
    foldlArray :: (b ~ Value EvalAnn, a ~ Value EvalAnn) => (b -> a -> Eval b) -> b -> Vector a -> Eval b
    foldlArray = foldM
    foldrArray :: (b ~ Value EvalAnn, a ~ Value EvalAnn) => (a -> b -> Eval b) -> b -> Vector a -> Eval b
    foldrArray = foldrM
    op1 :: (a -> b) -> a -> Eval b
    op1 op = pure . op
    op2 :: (a -> b -> c) -> a -> b -> Eval c
    op2 op x y = pure (op x y)
    op3 :: (a -> b -> c -> d) -> a -> b -> c -> Eval d
    op3 op x y z = pure (op x y z)
    readAs :: StringConv s Text => (Text -> Either s (a, Text)) -> Text -> Eval a
    readAs parse t = either (throwError . ForeignFunctionError Nothing . toS) (pure . fst) (parse t)
    readInt :: Int -> Text -> Eval Int
    readInt = \case
      10 -> readAs Text.decimal
      16 -> readAs Text.hexadecimal
      radix -> const (throwError (ForeignFunctionError Nothing ("Unsupported radix for readInt: " <> show radix)))
    eqArray :: (a ~ Value EvalAnn, b ~ Bool) => (a -> a -> Eval b) -> Vector a -> Vector a -> Eval b
    eqArray pred' v1 v2
      | Vector.length v1 == Vector.length v2 = Vector.and <$> Vector.zipWithM pred' v1 v2
      | otherwise = pure False
    ordImpl :: forall a o. (Show a, Ord a, o ~ Value EvalAnn) => o -> o -> o -> a -> a -> Eval o
    ordImpl lt eq gt x y = pure $ case x `compare` y of
      LT -> lt
      EQ -> eq
      GT -> gt
    intDegree :: Int -> Eval Int
    intDegree n = pure (min (abs n) 2147483647)
    intDiv :: Int -> Int -> Eval Int
    intDiv x y
      | y == 0 = pure 0
      | otherwise = pure (x `div` y)
    intMod :: Int -> Int -> Eval Int
    intMod x y
      | y == 0 = pure 0
      | otherwise = let yy = abs y in pure ((x `mod` yy) + yy `mod` yy)
    unfoldrArrayImpl ::
      (Value EvalAnn -> Eval Bool) -> -- isNothing
      (Value EvalAnn -> Eval (Value EvalAnn)) -> -- fromJust
      (Value EvalAnn -> Eval (Value EvalAnn)) -> -- fst
      (Value EvalAnn -> Eval (Value EvalAnn)) -> -- snd
      (Value EvalAnn -> Eval (Value EvalAnn)) -> -- f
      Value EvalAnn -> -- b
      Eval (Vector (Value EvalAnn))
    unfoldrArrayImpl isNothing' fromJust' fst' snd' f =
      Vector.unfoldrM $ \b -> do
        r <- f b
        isNothing' r >>= \case
          True -> pure Nothing
          False -> do
            tuple <- fromJust' r
            a <- fst' tuple
            b' <- snd' tuple
            pure (Just (a, b'))
    unsafePartial :: Value EvalAnn -> Eval (Value EvalAnn)
    unsafePartial f = do
      Function fenv _ body <- require nullSourceSpan (Proxy @"VFunction") f
      eval fenv body

evalForeignApply :: SourceSpan -> Env EvalAnn -> ApplyForeign -> Eval (Value EvalAnn)
evalForeignApply ss env (ApplyForeign qn paramNames) = do
  params <- for paramNames $ \n -> envLookupEval ss (Qualified Nothing n) env
  case Map.lookup qn foreignFunctions of
    (Just f) -> evalFn f ss env params
    _ -> throwError (ForeignFunctionNotSupported ss qn)
