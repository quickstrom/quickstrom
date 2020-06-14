{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module WebCheck.PureScript.Program where

import Control.Lens hiding (op)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Fixed (mod')
import Data.Generics.Product (field)
import Data.HashMap.Strict (HashMap)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Doc, defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import qualified Data.Text.Read as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import Language.PureScript.CoreFn hiding (Ann)
import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.Names
import Protolude hiding (moduleName)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import qualified Test.QuickCheck as QuickCheck
import Text.Read (read)
import qualified WebCheck.Action as WebCheck
import qualified WebCheck.Element as WebCheck
import qualified WebCheck.Path as WebCheck
import WebCheck.PureScript.Eval (ApplyForeign (..), Eval, EvalAnn, EvalAnn (..), EvalForeignFunction, MonadEval, envLookupEval, eval, evalAnnFromAnn, modifyEnv, qualifiedName, runEval)
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.ForeignFunction
import WebCheck.PureScript.Pretty
import qualified WebCheck.PureScript.Queries as Queries
import WebCheck.PureScript.Value
import qualified WebCheck.Result as WebCheck
import qualified WebCheck.Specification as WebCheck
import qualified WebCheck.Trace as WebCheck

initialEnv :: Env EvalAnn
initialEnv =
  foldMap bindForeignPair (Map.toList foreignFunctions)
  where
    builtInSS = P.internalModuleSourceSpan "<builtin>"
    bindForeignFunction :: Qualified Ident -> Int -> Env EvalAnn
    bindForeignFunction qn arity' =
      envBindExpr qn (wrap arity' (\names -> Var (EvalAnn builtInSS {P.spanName = toS (showQualified runIdent qn)} (Just IsForeign) (Just (ApplyForeign qn names))) qn))
    bindForeignPair :: (Qualified Ident, SomeForeignFunction (Eval Identity)) -> Env EvalAnn
    bindForeignPair (qn, SomeForeignFunction f) = bindForeignFunction qn (foreignFunctionArity f)
    wrap :: Int -> ([Ident] -> Expr EvalAnn) -> Expr EvalAnn
    wrap arity' f =
      let names = [Ident ("x" <> show n) | n <- [1 .. arity']]
       in foldr (Abs (EvalAnn builtInSS Nothing Nothing)) (f names) names

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
        (map ((,P.nullSourceSpan) . P.eiModule) (P.efImports e))

data Program m
  = Program
      { programLibraryModules :: Modules,
        programMain :: Module CF.Ann,
        programEnv :: Env EvalAnn,
        programForeignFunctions :: Map (Qualified Ident) (EvalForeignFunction m)
      }

programQualifiedName :: Text -> Program m -> Qualified Ident
programQualifiedName name p =
  Qualified (Just (moduleName (programMain p))) (Ident name)

toModuleEnv :: Module CF.Ann -> Env EvalAnn
toModuleEnv m =
  let addDecl = \case
        NonRec _ name expr -> bindExpr name expr
        Rec binds -> foldMap (\((_, name), expr) -> bindExpr name expr) binds
   in foldMap addDecl (moduleDecls m)
  where
    bindExpr name expr = envBindExpr (Qualified (Just (moduleName m)) name) (evalAnnFromAnn <$> expr)

loadProgram :: MonadFix m => Modules -> Text -> IO (Either Text (Program (Eval m)))
loadProgram ms input = runExceptT $ do
  specModule <- loadModuleFromSource ms input
  let env' = foldMap toModuleEnv (modulesCoreFn ms <> [specModule])
  pure
    ( Program
        { programLibraryModules = ms,
          programMain = specModule,
          programEnv = initialEnv <> env',
          programForeignFunctions = ffs
        }
    )
  where
    ffs :: MonadFix m => Map (Qualified Ident) (EvalForeignFunction (Eval m))
    ffs = map (\(SomeForeignFunction f) -> evalForeignFunction f) foreignFunctions

data SpecificationProgram
  = SpecificationProgram
      { specificationOrigin :: WebCheck.Path,
        specificationReadyWhen :: WebCheck.Selector,
        specificationActions :: [WebCheck.Action WebCheck.Selector],
        specificationQueries :: WebCheck.Queries,
        specificationProgram :: Program (Eval Identity)
      }

instance WebCheck.Specification SpecificationProgram where

  origin = specificationOrigin

  readyWhen = specificationReadyWhen

  actions = QuickCheck.elements . specificationActions

  verify sp states = (_Left %~ prettyEvalError) . runIdentity . runEval (programEnv p) states (programForeignFunctions p) $ do
    valid <- require (moduleSourceSpan (programMain p)) (Proxy @"VBool") =<< evalEntryPoint entry
    if valid then pure WebCheck.Accepted else pure WebCheck.Rejected
    where
      p = specificationProgram sp
      entry = programQualifiedName "proposition" p

  queries = specificationQueries

loadSpecification :: Modules -> Text -> IO (Either Text SpecificationProgram)
loadSpecification ms input = runExceptT $ do
  p <- ExceptT (loadProgram ms input)
  either (throwError . prettyText . prettyEvalErrorWithSourceSpan) pure . runIdentity . runEval (programEnv p) [mempty] (programForeignFunctions p) $ do
    let ss = (moduleSourceSpan (programMain p))
    origin <- toHaskellValue ss =<< evalEntryPoint (programQualifiedName "origin" p)
    readyWhen <- toHaskellValue ss =<< evalEntryPoint (programQualifiedName "readyWhen" p)
    actions <- toHaskellValue ss =<< evalEntryPoint (programQualifiedName "actions" p)
    queries <- do
      let qn = programQualifiedName "proposition" p
      case envLookup qn (programEnv p) of
        Just (Left expr) -> Queries.runExtract (eval expr)
        Just (Right _val) -> pure mempty
        Nothing -> throwError (NotInScope P.nullSourceSpan qn)
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

entrySS :: P.SourceSpan
entrySS = P.internalModuleSourceSpan "<entry>"

evalEntryPoint :: MonadEval m => Qualified Ident -> m (Value EvalAnn)
evalEntryPoint entryPoint = envLookupEval entrySS entryPoint

runWithEntryPoint :: ToHaskellValue (Eval Identity) a => [WebCheck.ObservedState] -> Qualified Ident -> Program (Eval Identity) -> IO (Either Text a)
runWithEntryPoint observedStates entry prog = runExceptT $ do
  case runIdentity (runEval (programEnv prog) observedStates (programForeignFunctions prog) (evalEntryPoint entry >>= toHaskellValue entrySS)) of
    Right value -> pure value
    Left err -> throwError (prettyText (prettyEvalErrorWithSourceSpan err))

prettyText :: Doc ann -> Text
prettyText x = renderStrict (layoutPretty defaultLayoutOptions x)

-- * Foreign Functions

evalForeignApply :: MonadFix m => P.SourceSpan -> ApplyForeign -> Eval m (Value EvalAnn)
evalForeignApply ss (ApplyForeign qn paramNames) = do
  params <- for paramNames $ \n -> envLookupEval ss (Qualified Nothing n)
  case Map.lookup qn foreignFunctions of
    (Just (SomeForeignFunction f)) -> evalForeignFunction f ss params
    _ -> throwError (ForeignFunctionNotSupported ss qn)

foreignFunctions :: MonadFix m => Map (Qualified Ident) (SomeForeignFunction (Eval m))
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
      (qualifiedName "Data.Show" "cons", foreignFunction (op2 Vector.cons :: forall m a. Monad m => a ~ Value EvalAnn => a -> Vector a -> Ret m (Vector a))),
      (qualifiedName "Data.Show" "join", foreignFunction (op2 Text.intercalate)),
      (qualifiedName "Data.Semiring" "intAdd", foreignFunction (op2 ((+) @Int))),
      (qualifiedName "Data.Semiring" "intMul", foreignFunction (op2 ((*) @Int))),
      (qualifiedName "Data.Semiring" "numAdd", foreignFunction (op2 ((+) @Double))),
      (qualifiedName "Data.Semiring" "numMul", foreignFunction (op2 ((*) @Double))),
      (qualifiedName "Data.Semigroup" "concatString", foreignFunction (op2 ((<>) @Text))),
      (qualifiedName "Data.Semigroup" "concatArray", foreignFunction (op2 (<>) :: forall m a. Monad m => a ~ Value EvalAnn => Vector a -> Vector a -> Ret m (Vector a))),
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
      (qualifiedName "Record.Unsafe" "unsafeGet", foreignFunction unsafeGet)
    ]
  where
    notSupported :: MonadError EvalError m => Qualified Ident -> (Qualified Ident, SomeForeignFunction m)
    notSupported qn = (qn, SomeForeignFunction (NotSupported qn))
    indexImpl :: (Monad m, a ~ Value EvalAnn) => (a -> m (Value EvalAnn)) -> Value EvalAnn -> Vector a -> Int -> Ret m (Value EvalAnn)
    indexImpl just nothing xs i = Ret (maybe (pure nothing) just (xs ^? ix (fromIntegral i)))
    fromNumberImpl :: (Int -> m (Value EvalAnn)) -> Value EvalAnn -> Double -> Ret m (Value EvalAnn)
    fromNumberImpl just _ x = Ret (just (round x))
    fromStringAsImpl :: Monad m => (Int -> m (Value EvalAnn)) -> Value EvalAnn -> Int -> Text -> Ret m (Value EvalAnn)
    fromStringAsImpl just nothing radix t =
      Ret . either (const (pure nothing)) (just . fst) $ case radix of
        10 -> Text.decimal t
        16 -> Text.hexadecimal t
        _ -> Left mempty
    len :: Monad m => Vector (Value EvalAnn) -> Ret m Int
    len xs = pure (fromIntegral (Vector.length xs))
    filterArray :: Monad m => (Value EvalAnn -> m Bool) -> Vector (Value EvalAnn) -> Ret m (Vector (Value EvalAnn))
    filterArray f xs = Ret (Vector.filterM f xs)
    arrayBind :: Monad m => (a ~ Value EvalAnn, b ~ Value EvalAnn) => Vector a -> (a -> m (Vector b)) -> Ret m (Vector b)
    arrayBind xs f = Ret (join <$> traverse f xs)
    arrayMap :: Monad m => (a ~ Value EvalAnn, b ~ Value EvalAnn) => (a -> m b) -> Vector a -> Ret m (Vector b)
    arrayMap f xs = Ret (Vector.mapM f xs)
    foldlArray :: Monad m => (b ~ Value EvalAnn, a ~ Value EvalAnn) => (b -> a -> m b) -> b -> Vector a -> Ret m b
    foldlArray f s xs = Ret (foldM f s xs)
    foldrArray :: Monad m => (b ~ Value EvalAnn, a ~ Value EvalAnn) => (a -> b -> m b) -> b -> Vector a -> Ret m b
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
    eqArray :: Monad m => (a ~ Value EvalAnn, b ~ Bool) => (a -> a -> m b) -> Vector a -> Vector a -> Ret m b
    eqArray pred' v1 v2
      | Vector.length v1 == Vector.length v2 = Ret (Vector.and <$> Vector.zipWithM pred' v1 v2)
      | otherwise = pure False
    ordImpl :: forall a o m. Monad m => (Show a, Ord a, o ~ Value EvalAnn) => o -> o -> o -> a -> a -> Ret m o
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
      (Value EvalAnn -> m Bool) -> -- isNothing
      (Value EvalAnn -> m (Value EvalAnn)) -> -- fromJust
      (Value EvalAnn -> m (Value EvalAnn)) -> -- fst
      (Value EvalAnn -> m (Value EvalAnn)) -> -- snd
      (Value EvalAnn -> m (Value EvalAnn)) -> -- f
      Value EvalAnn -> -- b
      Ret m (Vector (Value EvalAnn))
    unfoldrArrayImpl isNothing' fromJust' fst' snd' f =
      Ret
        . ( Vector.unfoldrM $ \b -> do
              r <- f b
              isNothing' r >>= \case
                True -> pure Nothing
                False -> do
                  tuple <- fromJust' r
                  a <- fst' tuple
                  b' <- snd' tuple
                  pure (Just (a, b'))
          )
    unsafePartial :: MonadEval m => Value EvalAnn -> Ret m (Value EvalAnn)
    unsafePartial f = Ret $ do
      Function fenv _ body <- require P.nullSourceSpan (Proxy @"VFunction") f
      modifyEnv (const fenv) (eval body)
    unsafeGet :: MonadError EvalError m => Text -> HashMap Text (Value EvalAnn) -> Ret m (Value EvalAnn)
    unsafeGet k xs = Ret (accessField P.nullSourceSpan k xs)
