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
{-# LANGUAGE NoImplicitPrelude #-}

module WebCheck.PureScript where

import Control.Lens hiding (op)
import Control.Monad.Except (liftEither)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Fixed (mod')
import Data.Generics.Product (field)
import Data.Generics.Sum (AsConstructor, _Ctor)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Data.Map as Map
import Data.Scientific
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
import qualified Test.QuickCheck as QuickCheck
import Text.Read (read)
import qualified WebCheck.Action as WebCheck
import qualified WebCheck.Element as WebCheck
import qualified WebCheck.Path as WebCheck
import WebCheck.PureScript.EvalError
import WebCheck.PureScript.Pretty
import WebCheck.PureScript.Value
import qualified WebCheck.Result as WebCheck
import qualified WebCheck.Specification as WebCheck
import qualified WebCheck.Trace as WebCheck

data EvalAnn = EvalAnn {annSourceSpan :: SourceSpan, annMeta :: Maybe Meta, annApplyForeign :: Maybe ApplyForeign}
  deriving (Show, Generic)

evalAnnFromAnn :: Ann -> EvalAnn
evalAnnFromAnn (ss, _, _, meta) = EvalAnn ss meta Nothing

data EvalEnv
  = EvalEnv
      { env :: Env EvalAnn,
        observedStates :: [(Int, WebCheck.ObservedState)]
        -- , evalQuery :: Env EvalAnn -> Expr EvalAnn -> Expr EvalAnn -> WebCheck.ObservedState -> Eval (Value EvalAnn)
      }
  deriving (Show, Generic)

newtype Eval a = Eval (ExceptT (EvalError EvalAnn) (Reader EvalEnv) a)
  deriving (Functor, Applicative, Monad, MonadError (EvalError EvalAnn), MonadFix, MonadReader EvalEnv)

runEval :: Env EvalAnn -> [WebCheck.ObservedState] -> Eval a -> Either (EvalError EvalAnn) a
runEval env observedStates (Eval ma) = runReader (runExceptT ma) (EvalEnv env (zip [1 ..] observedStates))

unexpectedType :: (MonadError (EvalError EvalAnn) m) => SourceSpan -> Text -> Value EvalAnn -> m a
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
  forall (ctor :: Symbol) s t a b ann m.
  ( KnownSymbol ctor,
    AsConstructor ctor s t a b,
    ann ~ EvalAnn,
    s ~ Value ann,
    t ~ Value ann,
    a ~ b,
    MonadError (EvalError EvalAnn) m
  ) =>
  SourceSpan ->
  Proxy ctor ->
  Value ann ->
  m b
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
  foldMap bindForeignPair (Map.toList foreignFunctions)
  where
    builtInSS = internalModuleSourceSpan "<builtin>"
    bindForeignFunction :: Qualified Ident -> Int -> Env EvalAnn
    bindForeignFunction qn arity' =
      envBindExpr qn (wrap arity' (\names -> Var (EvalAnn builtInSS {spanName = toS (showQualified runIdent qn)} (Just IsForeign) (Just (ApplyForeign qn names))) qn))
    bindForeignPair :: (Qualified Ident, ForeignFunction (Either (EvalError EvalAnn))) -> Env EvalAnn
    bindForeignPair (qn, f) = bindForeignFunction qn (arity f)
    wrap :: Int -> ([Ident] -> Expr EvalAnn) -> Expr EvalAnn
    wrap arity' f =
      let names = [Ident ("x" <> show n) | n <- [1 .. arity']]
       in foldr (Abs (EvalAnn builtInSS Nothing Nothing)) (f names) names

envLookupEval :: SourceSpan -> Qualified Ident -> Eval (Value EvalAnn)
envLookupEval ss qn = do
  env' <- asks env
  case envLookup qn env' of
    Just r -> either (local (field @"env" %~ withoutLocals) . eval) onValue r
    Nothing -> throwError (NotInScope ss qn)
  where
    onValue (VDefer (Defer env'' expr')) = local (field @"env" .~ env'') (eval expr')
    onValue val = pure val

withModifiedEnv f = local (field @"env" %~ f)

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

accessField :: MonadError (EvalError EvalAnn) m => SourceSpan -> Text -> HashMap Text (Value EvalAnn) -> m (Value EvalAnn)
accessField ss key obj =
  maybe
    (throwError (UnexpectedError (Just ss) ("Key not present in object: " <> key)))
    pure
    (HashMap.lookup key obj)

pattern BuiltIn :: Text -> a -> Expr a -> Expr a
pattern BuiltIn name ann p <- App ann (Var _ (Qualified (Just (ModuleName "WebCheck.DSL")) (Ident name))) p

pattern Always :: a -> Expr a -> Expr a
pattern Always ann p <- BuiltIn "always" ann p

pattern Next :: a -> Expr a -> Expr a
pattern Next ann p <- BuiltIn "next" ann p

eval :: Expr EvalAnn -> Eval (Value EvalAnn)
eval expr = do
  EvalEnv {env, observedStates} <- ask
  case observedStates of
    [] -> case expr of
      Always _ _ -> pure (VBool True)
      _ -> throwError Undetermined
    ((n, current) : rest) -> case expr of
      -- Special cases
      Next _ p -> local (field @"observedStates" .~ rest) (eval p)
      Always ann p -> do
        first' <- require (sourceSpan p) (Proxy @"VBool")
          =<< eval p `catchError` \case
            Undetermined -> pure (VBool True)
            e -> throwError e
        rest' <-
          require (annSourceSpan ann) (Proxy @"VBool")
            =<< local (field @"observedStates" %~ drop 1) (eval expr)
        pure (VBool (first' && rest'))
      App _ (BuiltIn "trace" _ label) p -> do
        t <- require (sourceSpan label) (Proxy @"VString") =<< eval label
        traceM
          ( prettyText
              ( prettySourceSpan (sourceSpan expr) <> colon
                  <+> "trace: in state"
                  <+> pretty n <> colon
                  <+> pretty t
              )
          )
        eval p
      App _ (BuiltIn "_queryAll" _ p) q -> evalQuery p q current
      BuiltIn "_property" _ p -> do
        name <- require (sourceSpan p) (Proxy @"VString") =<< eval p
        pure (VElementState (WebCheck.Property name))
      BuiltIn "_attribute" _ p -> do
        name <- require (sourceSpan p) (Proxy @"VString") =<< eval p
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
        func' <- require (sourceSpan func) (Proxy @"VFunction") =<< eval func
        param' <- eval param
        evalFunc func' param'
      Var _ (Qualified (Just (ModuleName "Prim")) (Ident "undefined")) -> pure (VObject mempty)
      Var (EvalAnn ss _ (Just applyForeign)) _ -> evalForeignApply ss applyForeign
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
        withModifiedEnv (<> newEnv) (eval body)

evalQuery :: Expr EvalAnn -> Expr EvalAnn -> WebCheck.ObservedState -> Eval (Value EvalAnn)
evalQuery p1 p2 (WebCheck.ObservedState current) = do
  selector <- require (sourceSpan p1) (Proxy @"VString") =<< eval p1
  wantedStates <- require (sourceSpan p2) (Proxy @"VObject") =<< eval p2
  matchedElements <-
    maybe
      (throwError (ForeignFunctionError (Just (sourceSpan p1)) ("Selector not in observed state: " <> selector)))
      pure
      (HashMap.lookup (WebCheck.Selector selector) current)
  mappedElements <- for (Vector.fromList matchedElements) $ \matchedElement -> do
    mappings <- flip HashMap.traverseWithKey wantedStates $ \k s -> do
      elementState <- require (sourceSpan p2) (Proxy @"VElementState") s
      case HashMap.lookup elementState matchedElement of
        Just x -> pure (fromValue x)
        Nothing ->
          let msg = ("Element state (bound to ." <> k <> ") not in observed state for query `" <> selector <> "`: " <> show elementState)
           in throwError (ForeignFunctionError (Just (sourceSpan p2)) msg)
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

evalFunc :: Function EvalAnn -> (Value EvalAnn) -> Eval (Value EvalAnn)
evalFunc (Function fEnv arg body) param' =
  let newEnv = (fEnv <> envBindValue (Qualified Nothing arg) param')
   in withModifiedEnv (const newEnv) (eval body)

evalCaseAlts :: SourceSpan -> [(Value EvalAnn)] -> [CaseAlternative EvalAnn] -> Eval (Value EvalAnn)
evalCaseAlts ss vals [] = throwError (UnexpectedError (Just ss) (prettyText ("Non-exhaustive case expression on values:" <+> pretty vals)))
evalCaseAlts ss values (CaseAlternative binders result : rest) =
  case envFromBinders (zip binders values) of
    Just bindersEnv ->
      case result of
        Left guardedExprs ->
          withModifiedEnv (<> bindersEnv) (evalGuards guardedExprs) >>= \case
            Just expr -> pure expr
            Nothing -> evalCaseAlts ss values rest
        Right expr -> withModifiedEnv (<> bindersEnv) (eval expr)
    Nothing -> evalCaseAlts ss values rest

evalGuards :: [(Guard EvalAnn, Expr EvalAnn)] -> Eval (Maybe (Value EvalAnn))
evalGuards [] = pure Nothing
evalGuards ((guard', branch) : rest') = do
  res <- require (sourceSpan guard') (Proxy @"VBool") =<< eval guard'
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
        (P.Module ss coms moduleName' elaborated exps, env') <- fmap fst . P.runSupplyT 0 $ do
          desugared <- P.desugar (modulesNamesEnv modules) (modulesExterns modules) [P.importPrim m] >>= \case
            [d] -> pure d
            _ -> throwError (P.MultipleErrors mempty)
          P.runCheck' (P.emptyCheckState (modulesInitEnv modules)) $ P.typeCheckModule desugared
        regrouped <- P.createBindingGroups moduleName' . P.collapseBindingGroups $ elaborated
        let mod'' = P.Module ss coms moduleName' regrouped exps
        pure (CF.moduleToCoreFn env' mod'')
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

programQualifiedName :: Text -> Program -> Qualified Ident
programQualifiedName name p =
  Qualified (Just (moduleName (programMain p))) (Ident name)

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

data SpecificationProgram
  = SpecificationProgram
      { specificationOrigin :: WebCheck.Path,
        specificationReadyWhen :: WebCheck.Selector,
        specificationActions :: [WebCheck.Action WebCheck.Selector],
        specificationQueries :: WebCheck.Queries,
        specificationProgram :: Program
      }

instance WebCheck.Specification SpecificationProgram where
  origin = specificationOrigin
  readyWhen = specificationReadyWhen
  actions = QuickCheck.elements . specificationActions
  verify sp states = (_Left %~ prettyEvalError) . runEval (programEnv p) states $ do
    valid <- require (moduleSourceSpan (programMain p)) (Proxy @"VBool") =<< evalEntryPoint entry p
    if valid then pure WebCheck.Accepted else pure WebCheck.Rejected
    where
      p = specificationProgram sp
      entry = programQualifiedName "proposition" p
  queries = specificationQueries

extractQueries :: Program -> Either (EvalError EvalAnn) (HashMap WebCheck.Selector (HashSet WebCheck.ElementState))
extractQueries prog = pure mempty -- TODO

{-
  let qn = programQualifiedName "proposition" prog
   in runEval (programEnv prog) [mempty] (runReaderT (extractQualified qn) Nothing)
  where
    env' = programEnv prog
    ss = moduleSourceSpan (programMain prog)
    extractQualified :: Qualified Ident -> ReaderT (Maybe (Qualified Ident)) (Eval) WebCheck.Queries
    extractQualified qn = do
      current <- ask
      if current == Just qn
        then pure mempty
        else case envLookup qn (programEnv prog) of
          Just (Left expr) -> local (const (Just qn)) (extractFromExpr expr)
          Just (Right (VDefer (Defer _ expr))) -> extractFromExpr expr
          Just (Right _) -> throwError (UnexpectedError (Just ss) ("Expected proposition to be an expression: " <> showQualified runIdent qn))
          Nothing -> pure mempty
    extractFromExpr :: Expr EvalAnn -> ReaderT (Maybe (Qualified Ident)) (Eval) WebCheck.Queries
    extractFromExpr = extract
      where
        (_, extract, _, _) =
          everythingOnValues
            (liftA2 (HashMap.unionWith (<>)))
            (const (pure mempty))
            ( \case
                App _ (BuiltIn "_queryAll" _ p) q -> lift $ do
                  selector <- require (sourceSpan p) (Proxy @"VString") =<< eval env' p
                  wantedStates <- require (sourceSpan q) (Proxy @"VObject") =<< eval env' q
                  elementStates <- for (HashMap.elems wantedStates) (require (sourceSpan q) (Proxy @"VElementState"))
                  pure (HashMap.singleton (WebCheck.Selector selector) (HashSet.fromList elementStates))
                Var _ qn -> extractQualified qn
                _ -> pure mempty
            )
            (const (pure mempty))
            (const (pure mempty))
-}

loadSpecification :: Modules -> Text -> IO (Either Text SpecificationProgram)
loadSpecification ms input = runExceptT $ do
  p <- ExceptT (loadProgram ms input)
  either (throwError . prettyText . prettyEvalErrorWithSourceSpan) pure . runEval (programEnv p) [mempty] $ do
    let ss = (moduleSourceSpan (programMain p))
    origin <- toHaskellValue ss =<< evalEntryPoint (programQualifiedName "origin" p) p
    readyWhen <- toHaskellValue ss =<< evalEntryPoint (programQualifiedName "readyWhen" p) p
    actions <- toHaskellValue ss =<< evalEntryPoint (programQualifiedName "actions" p) p
    queries <- liftEither (extractQueries p)
    pure
      ( SpecificationProgram
          { specificationOrigin = WebCheck.Path origin,
            specificationReadyWhen = WebCheck.Selector readyWhen,
            specificationActions = actions,
            specificationQueries = queries,
            specificationProgram = p
          }
      )

loadSpecificationFile :: Modules -> FilePath -> IO (Either Text SpecificationProgram)
loadSpecificationFile ms input = loadSpecification ms =<< readFile input

entrySS :: SourceSpan
entrySS = internalModuleSourceSpan "<entry>"

evalEntryPoint :: Qualified Ident -> Program -> Eval (Value EvalAnn)
evalEntryPoint entryPoint prog = envLookupEval entrySS entryPoint

runWithEntryPoint :: forall a. ToHaskellValue a => [WebCheck.ObservedState] -> Qualified Ident -> Program -> IO (Either Text a)
runWithEntryPoint observedStates entry prog = runExceptT $ do
  case runEval (programEnv prog) observedStates (evalEntryPoint entry prog >>= toHaskellValue entrySS) of
    Right value -> pure value
    Left err -> throwError (prettyText (prettyEvalErrorWithSourceSpan err))

prettyText :: Doc ann -> Text
prettyText x = renderStrict (layoutPretty defaultLayoutOptions x)

-- * Foreign Functions

data ApplyForeign = ApplyForeign (Qualified Ident) [Ident]
  deriving (Show, Generic)

evalForeignApply :: SourceSpan -> ApplyForeign -> Eval (Value EvalAnn)
evalForeignApply ss (ApplyForeign qn paramNames) = do
  params <- for paramNames $ \n -> envLookupEval ss (Qualified Nothing n)
  case Map.lookup qn foreignFunctions of
    (Just (ForeignFunction {evalFn})) -> evalFn ss params
    _ -> throwError (ForeignFunctionNotSupported ss qn)
