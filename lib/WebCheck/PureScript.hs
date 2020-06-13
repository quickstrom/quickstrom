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

module WebCheck.PureScript where

import Control.Lens hiding (op)
import Control.Monad.Except (liftEither)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Fixed (mod')
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
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
import Language.PureScript.AST (SourceSpan, internalModuleSourceSpan, nullSourceSpan, spanName)
import qualified Language.PureScript.CST as CST
import Language.PureScript.CoreFn hiding (Ann)
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
import WebCheck.PureScript.Eval.Class
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.ForeignFunction
import WebCheck.PureScript.Pretty
import WebCheck.PureScript.Value
import qualified WebCheck.Result as WebCheck
import qualified WebCheck.Specification as WebCheck
import qualified WebCheck.Trace as WebCheck

data EvalAnn = EvalAnn {annSourceSpan :: SourceSpan, annMeta :: Maybe Meta, annApplyForeign :: Maybe ApplyForeign}
  deriving (Show, Generic)

evalAnnFromAnn :: CF.Ann -> EvalAnn
evalAnnFromAnn (ss, _, _, meta) = EvalAnn ss meta Nothing

data EvalEnv
  = EvalEnv
      { env :: Env EvalAnn,
        observedStates :: [(Int, WebCheck.ObservedState)]
        -- , evalQuery :: Env EvalAnn -> Expr EvalAnn -> Expr EvalAnn -> WebCheck.ObservedState -> Eval (Value EvalAnn)
      }
  deriving (Show, Generic)

newtype Eval a = Eval (ExceptT EvalError (Reader EvalEnv) a)
  deriving (Functor, Applicative, Monad, MonadError EvalError, MonadFix, MonadReader EvalEnv)

runEval :: Env EvalAnn -> [WebCheck.ObservedState] -> Eval a -> Either EvalError a
runEval env observedStates (Eval ma) = runReader (runExceptT ma) (EvalEnv env (zip [1 ..] observedStates))

sourceSpan :: Expr EvalAnn -> SourceSpan
sourceSpan = annSourceSpan . extractAnn

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
    bindForeignPair :: (Qualified Ident, SomeForeignFunction m) -> Env EvalAnn
    bindForeignPair (qn, SomeForeignFunction f) = bindForeignFunction qn (foreignFunctionArity f)
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

pattern BuiltIn :: Text -> a -> Expr a -> Expr a
pattern BuiltIn name ann p <- App ann (Var _ (Qualified (Just (ModuleName "WebCheck.DSL")) (Ident name))) p

pattern Always :: a -> Expr a -> Expr a
pattern Always ann p <- BuiltIn "always" ann p

pattern Next :: a -> Expr a -> Expr a
pattern Next ann p <- BuiltIn "next" ann p

instance MonadEval Eval where
  type Ann Eval = EvalAnn
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

  evalFunc (Function fEnv arg body) param' =
    let newEnv = (fEnv <> envBindValue (Qualified Nothing arg) param')
    in withModifiedEnv (const newEnv) (eval body)


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

toModuleEnv :: Module CF.Ann -> Env EvalAnn
toModuleEnv m =
  let addDecl = \case
        NonRec _ name expr -> bindExpr name expr
        Rec binds -> foldMap (\((_, name), expr) -> bindExpr name expr) binds
   in foldMap addDecl (moduleDecls m)
  where
    bindExpr name expr = envBindExpr (Qualified (Just (moduleName m)) name) (evalAnnFromAnn <$> expr)

loadModuleFromSource :: Modules -> Text -> ExceptT Text IO (Module CF.Ann)
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

loadModuleFromCoreFn :: FilePath -> ExceptT Text IO (Module CF.Ann)
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
    addNameToDecl :: Text -> Bind CF.Ann -> Bind CF.Ann
    addNameToDecl name = fmap (_1 . field @"spanName" .~ toS name)

data Modules
  = Modules
      { modulesCoreFn :: [Module CF.Ann],
        modulesExterns :: [P.ExternsFile],
        modulesNamesEnv :: P.Env,
        modulesInitEnv :: P.Environment
      }
  deriving (Show)

loadModulesFromCoreFn :: FilePath -> ExceptT Text IO [Module CF.Ann]
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
        programMain :: Module CF.Ann,
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

extractQueries :: Program -> Either EvalError (HashMap WebCheck.Selector (HashSet WebCheck.ElementState))
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

runWithEntryPoint :: forall a. ToHaskellValue Eval a => [WebCheck.ObservedState] -> Qualified Ident -> Program -> IO (Either Text a)
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
    (Just (SomeForeignFunction f)) -> evalForeignFunction f ss params
    _ -> throwError (ForeignFunctionNotSupported ss qn)

foreignFunctions :: Map (Qualified Ident) (SomeForeignFunction Eval)
foreignFunctions =
  Map.fromList
    [ (qualifiedName "Control.Bind" "arrayBind", foreignFunction arrayBind),
      (qualifiedName "Data.Array" "indexImpl", foreignFunction indexImpl),
      (qualifiedName "Data.Array" "length", foreignFunction len),
      (qualifiedName "Data.Array" "filter", foreignFunction filterArray),
      (qualifiedName "Data.Bounded" "bottomInt", foreignFunction (op0 @Int minBound)),
      (qualifiedName "Data.Bounded" "topInt", foreignFunction (op0 @Int maxBound)),
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
      (qualifiedName "Data.HeytingAlgebra" "boolNot", foreignFunction (op1 not)),
      (qualifiedName "Data.Int" "toNumber", foreignFunction (op1 (fromIntegral @Int @Double))),
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
      (qualifiedName "Data.Show" "cons", foreignFunction (op2 Vector.cons :: forall m a. Monad m => a ~ Value (Ann m) => a -> Vector a -> Ret m (Vector a))),
      (qualifiedName "Data.Show" "join", foreignFunction (op2 Text.intercalate)),
      (qualifiedName "Data.Semiring" "intAdd", foreignFunction (op2 ((+) @Int))),
      (qualifiedName "Data.Semiring" "intMul", foreignFunction (op2 ((*) @Int))),
      (qualifiedName "Data.Semiring" "numAdd", foreignFunction (op2 ((+) @Double))),
      (qualifiedName "Data.Semiring" "numMul", foreignFunction (op2 ((*) @Double))),
      (qualifiedName "Data.Semigroup" "concatString", foreignFunction (op2 ((<>) @Text))),
      (qualifiedName "Data.Semigroup" "concatArray", foreignFunction (op2 (<>) :: forall m a. Monad m => a ~ Value (Ann m) => Vector a -> Vector a -> Ret m (Vector a))),
      notSupported (qualifiedName "Data.String.Common" "_localeCompare"),
      (qualifiedName "Data.String.Common" "replace", foreignFunction (op3 Text.replace)),
      (qualifiedName "Data.String.Common" "split", foreignFunction (op2 Text.splitOn)),
      (qualifiedName "Data.String.Common" "toLower", foreignFunction (op1 Text.toLower)),
      (qualifiedName "Data.String.Common" "toUpper", foreignFunction (op1 Text.toUpper)),
      (qualifiedName "Data.String.Common" "trim", foreignFunction (op1 Text.strip)),
      (qualifiedName "Data.String.Common" "joinWith", foreignFunction (op2 Text.intercalate)),
      (qualifiedName "Data.Unfoldable" "unfoldrArrayImpl", foreignFunction unfoldrArrayImpl),
      (qualifiedName "Global" "infinity", foreignFunction (op0 (read "Infinity" :: Double))),
      (qualifiedName "Global" "nan", foreignFunction (op0 (read "NaN" :: Double))),
      (qualifiedName "Global" "isFinite", foreignFunction (op1 (not . isInfinite @Double))),
      (qualifiedName "Global" "readFloat", foreignFunction (readAs Text.double)),
      (qualifiedName "Global" "readInt", foreignFunction readInt),
      (qualifiedName "Math" "floor", foreignFunction (op1 (fromIntegral @Int @Double . floor @Double @Int))),
      (qualifiedName "Math" "remainder", foreignFunction (op2 (mod' @Double))),
      (qualifiedName "Partial.Unsafe" "unsafePartial", foreignFunction unsafePartial),
      (qualifiedName "Record.Unsafe" "unsafeGet", foreignFunction (\k xs -> Ret (accessField nullSourceSpan k xs)))
    ]
  where
    notSupported :: MonadError EvalError m => Qualified Ident -> (Qualified Ident, SomeForeignFunction m)
    notSupported qn = (qn, SomeForeignFunction (NotSupported qn))
    indexImpl :: Monad m => a ~ (Value (Ann m)) => (a -> m (Value (Ann m))) -> Value (Ann m) -> Vector a -> Int -> Ret m (Value (Ann m))
    indexImpl just nothing xs i = Ret (maybe (pure nothing) just (xs ^? ix (fromIntegral i)))
    fromNumberImpl :: (Int -> m (Value (Ann m))) -> Value (Ann m) -> Double -> Ret m (Value (Ann m))
    fromNumberImpl just _ x = Ret (just (round x))
    fromStringAsImpl :: Monad m => (Int -> m (Value (Ann m))) -> Value (Ann m) -> Int -> Text -> Ret m (Value (Ann m))
    fromStringAsImpl just nothing radix t =
      Ret . either (const (pure nothing)) (just . fst) $ case radix of
        10 -> Text.decimal t
        16 -> Text.hexadecimal t
        _ -> Left mempty
    len :: Monad m => Vector (Value (Ann m)) -> Ret m Int
    len xs = pure (fromIntegral (Vector.length xs))
    filterArray :: Monad m => (Value (Ann m) -> m Bool) -> Vector (Value (Ann m)) -> Ret m (Vector (Value (Ann m)))
    filterArray f xs = Ret (Vector.filterM f xs)
    arrayBind :: MonadEval m => (a ~ Value ann, b ~ Value ann) => Vector a -> (a -> m (Vector b)) -> Ret m (Vector b)
    arrayBind xs f = Ret (join <$> traverse f xs)
    arrayMap :: Monad m => (a ~ Value (Ann m), b ~ Value (Ann m)) => (a -> m b) -> Vector a -> Ret m (Vector b)
    arrayMap f xs = Ret (Vector.mapM f xs)
    foldlArray :: Monad m => (b ~ Value (Ann m), a ~ Value (Ann m)) => (b -> a -> m b) -> b -> Vector a -> Ret m b
    foldlArray f s xs = Ret (foldM f s xs)
    foldrArray :: Monad m => (b ~ Value (Ann m), a ~ Value (Ann m)) => (a -> b -> m b) -> b -> Vector a -> Ret m b
    foldrArray f s xs = Ret (foldrM f s xs)
    op0 :: forall a m. Monad m => a -> Ret m a
    op0 = pure
    op1 :: forall a b m. Monad m => (a -> b) -> a -> Ret m b
    op1 op = pure . op
    op2 :: forall a b c m. Monad m => (a -> b -> c) -> a -> b -> Ret m c
    op2 op x y = pure (op x y)
    op3 :: forall a b c d m. Monad m => (a -> b -> c -> d) -> a -> b -> c -> Ret m d
    op3 op x y z = pure (op x y z)
    readAs :: (MonadEval m, StringConv s Text) => (Text -> Either s (a, Text)) -> Text -> Ret m a
    readAs parse t = Ret (either (throwError . ForeignFunctionError Nothing . toS) (pure . fst) (parse t))
    readInt :: MonadEval m => Int -> Text -> Ret m Int
    readInt = \case
      10 -> readAs Text.decimal
      16 -> readAs Text.hexadecimal
      radix -> const (Ret $ throwError (ForeignFunctionError Nothing ("Unsupported radix for readInt: " <> show radix)))
    eqArray :: Monad m => (a ~ Value (Ann m), b ~ Bool) => (a -> a -> m b) -> Vector a -> Vector a -> Ret m b
    eqArray pred' v1 v2
      | Vector.length v1 == Vector.length v2 = Ret (Vector.and <$> Vector.zipWithM pred' v1 v2)
      | otherwise = pure False
    ordImpl :: forall a o m. Monad m => (Show a, Ord a, o ~ Value (Ann m)) => o -> o -> o -> a -> a -> Ret m o
    ordImpl lt eq gt x y = pure $ case x `compare` y of
      LT -> lt
      EQ -> eq
      GT -> gt
    intDegree :: Monad m => Int -> Ret m Int
    intDegree n = pure (min (abs n) 2147483647)
    intDiv :: Monad m => Int -> Int -> Ret m Int
    intDiv x y
      | y == 0 = pure 0
      | otherwise = pure (x `div` y)
    intMod :: Monad m => Int -> Int -> Ret m Int
    intMod x y
      | y == 0 = pure 0
      | otherwise = let yy = abs y in pure ((x `mod` yy) + yy `mod` yy)
    unfoldrArrayImpl ::
      Monad m => 
      (Value (Ann m) -> m Bool) -> -- isNothing
      (Value (Ann m) -> m (Value (Ann m))) -> -- fromJust
      (Value (Ann m) -> m (Value (Ann m))) -> -- fst
      (Value (Ann m) -> m (Value (Ann m))) -> -- snd
      (Value (Ann m) -> m (Value (Ann m))) -> -- f
      Value (Ann m) -> -- b
      Ret m (Vector (Value (Ann m)))
    unfoldrArrayImpl isNothing' fromJust' fst' snd' f =
      Ret . (
      Vector.unfoldrM $ \b -> do
        r <- f b
        isNothing' r >>= \case
          True -> pure Nothing
          False -> do
            tuple <- fromJust' r
            a <- fst' tuple
            b' <- snd' tuple
            pure (Just (a, b'))
      )
    unsafePartial :: m ~ Eval => Value (Ann m) -> Ret m (Value (Ann m))
    unsafePartial f = Ret $ do
      Function fenv _ body <- require nullSourceSpan (Proxy @"VFunction") f
      withModifiedEnv (const fenv) (eval body)