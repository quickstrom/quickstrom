{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Quickstrom.PureScript.Program where

import Control.Lens hiding (op)
import Control.Monad.Except (liftEither)
import Control.Monad.ListM (sortByM)
import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Fixed (mod')
import Data.Generics.Product (field)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Internal.Search as Text
import qualified Data.Text.Read as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import Language.PureScript.CoreFn hiding (Ann)
import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import qualified Quickstrom.Action as Quickstrom
import qualified Quickstrom.Element as Quickstrom
import Quickstrom.Prelude hiding (moduleName, uncons)
import qualified Quickstrom.PureScript.Analyze as Analyze
import Quickstrom.PureScript.Eval
import Quickstrom.PureScript.ForeignFunction
import Quickstrom.PureScript.Pretty
import Quickstrom.PureScript.Value
import qualified Quickstrom.Result as Quickstrom
import qualified Quickstrom.Specification as Quickstrom
import qualified Quickstrom.Trace as Quickstrom
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import Text.Read (read)

initialEnv :: Eval r m => Env' m
initialEnv =
  foldMap bindForeignPair (HashMap.toList foreignFunctions)
  where
    builtInSS = P.internalModuleSourceSpan "<builtin>"
    bindForeignFunction :: QualifiedName -> Int -> Env' m
    bindForeignFunction qn arity' =
      envBindTopLevel
        qn
        ( wrap
            arity'
            ( \names ->
                Var
                  (EvalAnn builtInSS (Just IsForeign) (Just (ApplyForeign qn (fromIdent <$> names))))
                  (toQualifiedIdent (Left qn))
            )
        )
    bindForeignPair :: (QualifiedName, SomeForeignFunction m) -> Env' m
    bindForeignPair (qn, SomeForeignFunction f) = bindForeignFunction qn (foreignFunctionArity f)
    wrap :: Int -> ([P.Ident] -> Expr EvalAnn) -> Expr EvalAnn
    wrap arity' f =
      let names = [P.Ident ("x" <> show n) | n <- [1 .. arity']]
       in foldr (Abs (EvalAnn builtInSS Nothing Nothing)) (f names) names

loadModuleFromSource :: Modules -> Text -> ExceptT Text IO (Module CF.Ann)
loadModuleFromSource modules input =
  case CST.parseModuleFromFile "<file>" input >>= CST.resFull of
    Left parseError -> throwError (printErrors (CST.toMultipleErrors "<file>" parseError))
    Right m -> do
      (result, _) <- withExceptT printErrors . runWriterT . flip runReaderT P.defaultOptions $ do
        (P.Module ss coms moduleName' elaborated exps, env') <- fmap fst . P.runSupplyT 0 $ do
          desugared <-
            P.desugar (modulesNamesEnv modules) (modulesExterns modules) [P.importPrim m] >>= \case
              [d] -> pure d
              _ -> throwError (P.MultipleErrors mempty)
          P.runCheck' (P.emptyCheckState (modulesInitEnv modules)) $ P.typeCheckModule desugared
        regrouped <- P.createBindingGroups moduleName' . P.collapseBindingGroups $ elaborated
        let mod'' = P.Module ss coms moduleName' regrouped exps
        pure (CF.moduleToCoreFn env' mod'')
      pure result
  where
    printErrors :: P.MultipleErrors -> Text
    printErrors errs = toS (P.prettyPrintMultipleErrors P.defaultPPEOptions (errs))

loadModuleFromCoreFn :: FilePath -> ExceptT Text IO (Module CF.Ann)
loadModuleFromCoreFn path = do
  j <- liftIO (BS.readFile path)
  case JSON.decode j of
    Just val ->
      case JSON.parse moduleFromJSON val of
        JSON.Success (_, m) ->
          pure m {moduleDecls = map (addNameToDecl (toS (modulePath m))) (moduleDecls m)}
        JSON.Error e -> throwError (toS e)
    Nothing -> throwError "Couldn't read CoreFn file."
  where
    addNameToDecl :: Text -> Bind CF.Ann -> Bind CF.Ann
    addNameToDecl name = fmap (_1 . field @"spanName" .~ toS name)

data Modules = Modules
  { modulesCoreFn :: [Module CF.Ann],
    modulesExterns :: [P.ExternsFile],
    modulesNamesEnv :: P.Env,
    modulesInitEnv :: P.Environment
  }
  deriving (Show)

loadModulesFromCoreFn :: FilePath -> ExceptT Text IO [Module CF.Ann]
loadModulesFromCoreFn quickstromPursDir = do
  let coreFnPath :: Text -> FilePath
      coreFnPath mn' = quickstromPursDir </> toS mn' </> "corefn.json"
  paths <- liftIO (glob (coreFnPath "*"))
  traverse loadModuleFromCoreFn paths

loadExterns :: P.ModuleName -> FilePath -> ExceptT Text IO P.ExternsFile
loadExterns (P.ModuleName mn) quickstromPursDir = do
  let path = quickstromPursDir </> toS mn </> "externs.cbor"
  withExceptT show (P.readExternsFile path) >>= \case
    Just ext -> pure ext
    Nothing -> throwError ("Could not read externs file: " <> toS path)

loadLibraryModules :: FilePath -> IO (Either Text Modules)
loadLibraryModules quickstromPursDir = runExceptT $ do
  libModules <- loadModulesFromCoreFn quickstromPursDir
  externs <- for libModules $ \m -> loadExterns (moduleName m) quickstromPursDir
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

data Program m = Program
  { programLibraryModules :: Modules,
    programMain :: Module CF.Ann,
    programEnv :: Env' m
  }

moduleQualifiedName :: P.ModuleName -> P.Ident -> Either EvalError QualifiedName
moduleQualifiedName mn name =
  case fromQualifiedIdent (P.Qualified (Just mn) name) of
    Left qn -> pure qn
    Right _ -> throwError (InvalidEntryPoint (fromIdent name))

programQualifiedName :: Text -> Program m -> Either EvalError QualifiedName
programQualifiedName name p = moduleQualifiedName (moduleName (programMain p)) (P.Ident name)

toModuleEnv :: Module CF.Ann -> Either EvalError (Env' m)
toModuleEnv m =
  let addDecl = \case
        NonRec _ name expr -> bindExpr name expr
        Rec binds -> fold <$> traverse (\((_, name), expr) -> bindExpr name expr) binds
   in fold <$> traverse addDecl (moduleDecls m)
  where
    bindExpr :: P.Ident -> Expr CF.Ann -> Either EvalError (Env' m)
    bindExpr name expr = do
      qn <- moduleQualifiedName (moduleName m) name
      pure (envBindTopLevel qn (evalAnnFromAnn <$> expr))

loadProgram ::
  Eval r m =>
  Modules ->
  Text ->
  IO (Either Text (Program m))
loadProgram ms input = runExceptT $ do
  specModule <- loadModuleFromSource ms input
  env' <-
    (fold <$> traverse toModuleEnv (modulesCoreFn ms <> [specModule]))
      & _Left %~ (prettyText . prettyEvalError)
      & liftEither
  let ffEnv = mempty {envForeignFunctions = ffs}
  pure
    ( Program
        { programLibraryModules = ms,
          programMain = specModule,
          programEnv = initialEnv <> env' <> ffEnv
        }
    )
  where
    ffs :: Eval r m => HashMap QualifiedName (EvalForeignFunction m EvalAnn)
    ffs = map (\(SomeForeignFunction f) -> EvalForeignFunction (evalForeignFunction f)) foreignFunctions

data SpecificationProgram = SpecificationProgram
  { specificationReadyWhen :: Quickstrom.Selector,
    specificationActions :: Vector (Int, Quickstrom.PotentialAction),
    specificationQueries :: Quickstrom.Queries,
    specificationProgram :: Program WithObservedStates
  }

instance Quickstrom.Specification SpecificationProgram where
  readyWhen = specificationReadyWhen

  actions = specificationActions

  verify sp states = (_Left %~ (prettyText . prettyEvalError)) $ do
    valid <- toHaskellValue (moduleSourceSpan (programMain p)) =<< evalWithObservedStates p "proposition" states
    if valid then pure Quickstrom.Accepted else pure Quickstrom.Rejected
    where
      p = specificationProgram sp

  queries = specificationQueries

loadSpecification :: Modules -> Text -> IO (Either Text SpecificationProgram)
loadSpecification ms input = runExceptT $ do
  p <- ExceptT (loadProgram ms input)
  p2 <- ExceptT (loadProgram ms input) -- temporary hack!
  either (throwError . prettyText . prettyEvalErrorWithSourceSpan) pure $ do
    let ss = (moduleSourceSpan (programMain p))
    readyWhen <- toHaskellValue ss =<< evalWithObservedStates p "readyWhen" []
    actions <- toHaskellValue ss =<< evalWithObservedStates p "actions" []
    queries <- extractQueries p2 "proposition"
    pure
      ( SpecificationProgram
          { specificationReadyWhen = Quickstrom.Selector readyWhen,
            specificationActions = Quickstrom.actionSumsToPotentialActions actions,
            specificationQueries = queries,
            specificationProgram = p
          }
      )

loadSpecificationFile :: Modules -> FilePath -> IO (Either Text SpecificationProgram)
loadSpecificationFile ms input = loadSpecification ms =<< readFile input

evalWithObservedStates ::
  Program WithObservedStates ->
  Text ->
  [Quickstrom.ObservedState] ->
  Either EvalError (Value EvalAnn)
evalWithObservedStates p n states = do
  qn <- programQualifiedName n p
  runWithObservedStates
    (programEnv p)
    states
    (evalEntryPoint qn)

extractQueries :: Program Analyze.SimpleEval -> Text -> Either EvalError Quickstrom.Queries
extractQueries p n = do
  qn <- programQualifiedName n p
  Analyze.runExtract
    (programEnv p)
    (Analyze.extractEntryPoint entrySS qn)

entrySS :: P.SourceSpan
entrySS = P.internalModuleSourceSpan "<entry>"

evalEntryPoint :: Eval r m => QualifiedName -> m (Value EvalAnn)
evalEntryPoint entryPoint = envLookupEval entrySS (Left entryPoint)

-- * Foreign Functions

foreignFunctions :: Eval r m => HashMap QualifiedName (SomeForeignFunction m)
foreignFunctions =
  HashMap.fromList
    [ (ffName "Control.Apply" "arrayApply", foreignFunction arrayApply),
      (ffName "Control.Bind" "arrayBind", foreignFunction (arrayBind @(Value EvalAnn) @(Value EvalAnn))),
      (ffName "Data.Array" "indexImpl", foreignFunction indexImpl),
      (ffName "Data.Array" "sortImpl", foreignFunction (sortImpl @(Value EvalAnn))),
      (ffName "Data.Array" "length", foreignFunction len),
      (ffName "Data.Array" "filter", foreignFunction filterArray),
      (ffName "Data.Array" "uncons'", foreignFunction arrayUncons),
      (ffName "Data.Array" "range", foreignFunction arrayRange),
      (ffName "Data.Array" "zipWith", foreignFunction (arrayZipWith @(Value EvalAnn) @(Value EvalAnn) @(Value EvalAnn))),
      (ffName "Data.Bounded" "bottomInt", foreignFunction (op0 @Int minBound)),
      (ffName "Data.Bounded" "topInt", foreignFunction (op0 @Int maxBound)),
      (ffName "Data.Bounded" "bottomChar", foreignFunction (op0 @Char minBound)),
      (ffName "Data.Bounded" "topChar", foreignFunction (op0 @Char maxBound)),
      (ffName "Data.Bounded" "bottomNumber", foreignFunction (op0 @Double 9007199254740991)), -- Number.MAX_SAFE_INTEGER in JS
      (ffName "Data.Bounded" "topNumber", foreignFunction (op0 @Double (-9007199254740991))), -- Number.MIN_SAFE_INTEGER in JS
      (ffName "Data.Enum" "toCharCode", foreignFunction (op1 ord)),
      (ffName "Data.Enum" "fromCharCode", foreignFunction (op1 chr)),
      (ffName "Data.Eq" "eqBooleanImpl", foreignFunction (op2 ((==) @Bool))),
      (ffName "Data.Eq" "eqIntImpl", foreignFunction (op2 ((==) @Int))),
      (ffName "Data.Eq" "eqNumberImpl", foreignFunction (op2 ((==) @Double))),
      (ffName "Data.Eq" "eqCharImpl", foreignFunction (op2 ((==) @Char))),
      (ffName "Data.Eq" "eqStringImpl", foreignFunction (op2 ((==) @Text))),
      (ffName "Data.Eq" "eqArrayImpl", foreignFunction (eqArray @(Value EvalAnn))),
      (ffName "Data.EuclideanRing" "intDegree", foreignFunction intDegree),
      (ffName "Data.EuclideanRing" "intDiv", foreignFunction intDiv),
      (ffName "Data.EuclideanRing" "intMod", foreignFunction intMod),
      (ffName "Data.EuclideanRing" "numDiv", foreignFunction (op2 @Double (/))),
      (ffName "Data.Foldable" "foldlArray", foreignFunction (foldlArray @(Value EvalAnn) @(Value EvalAnn))),
      (ffName "Data.Foldable" "foldrArray", foreignFunction (foldrArray @(Value EvalAnn) @(Value EvalAnn))),
      (ffName "Data.Functor" "arrayMap", foreignFunction (arrayMap @(Value EvalAnn) @(Value EvalAnn))),
      (ffName "Data.HeytingAlgebra" "boolConj", foreignFunction (op2 (&&))),
      (ffName "Data.HeytingAlgebra" "boolDisj", foreignFunction (op2 (||))),
      (ffName "Data.HeytingAlgebra" "boolNot", foreignFunction (op1 not)),
      (ffName "Data.Int" "toNumber", foreignFunction (op1 (fromIntegral @Int @Double))),
      (ffName "Data.Int" "fromNumberImpl", foreignFunction fromNumberImpl),
      (ffName "Data.Int" "fromStringAsImpl", foreignFunction fromStringAsImpl),
      (ffName "Data.Lazy" "defer", foreignFunction lazyDefer),
      (ffName "Data.Lazy" "force", foreignFunction lazyForce),
      (ffName "Data.Ord" "ordBooleanImpl", foreignFunction (ordImpl @Bool @(Value EvalAnn))),
      (ffName "Data.Ord" "ordIntImpl", foreignFunction (ordImpl @Int @(Value EvalAnn))),
      (ffName "Data.Ord" "ordNumberImpl", foreignFunction (ordImpl @Double @(Value EvalAnn))),
      (ffName "Data.Ord" "ordStringImpl", foreignFunction (ordImpl @Text @(Value EvalAnn))),
      (ffName "Data.Ord" "ordCharImpl", foreignFunction (ordImpl @Char @(Value EvalAnn))),
      (ffName "Data.Ring" "intSub", foreignFunction (op2 ((-) @Int))),
      (ffName "Data.Ring" "numSub", foreignFunction (op2 ((-) @Double))),
      (ffName "Data.Show" "showStringImpl", foreignFunction (op1 (show @Text @Text))),
      (ffName "Data.Show" "showIntImpl", foreignFunction (op1 (show @Int @Text))),
      (ffName "Data.Show" "showNumberImpl", foreignFunction (op1 (show @Double @Text))),
      (ffName "Data.Show" "cons", foreignFunction (op2 (Vector.cons @(Vector (Value EvalAnn))))),
      (ffName "Data.Show" "join", foreignFunction (op2 Text.intercalate)),
      (ffName "Data.Semiring" "intAdd", foreignFunction (op2 ((+) @Int))),
      (ffName "Data.Semiring" "intMul", foreignFunction (op2 ((*) @Int))),
      (ffName "Data.Semiring" "numAdd", foreignFunction (op2 ((+) @Double))),
      (ffName "Data.Semiring" "numMul", foreignFunction (op2 ((*) @Double))),
      (ffName "Data.Semigroup" "concatString", foreignFunction (op2 ((<>) @Text))),
      (ffName "Data.Semigroup" "concatArray", foreignFunction (op2 ((<>) @(Vector (Value EvalAnn))))),
      (ffName "Data.String.CodePoints" "_unsafeCodePointAt0", foreignFunction unsafeCodePointAt0),
      (ffName "Data.String.CodePoints" "_toCodePointArray", foreignFunction toCodePointArray),
      (ffName "Data.String.CodeUnits" "_indexOf", foreignFunction codeUnitsIndexOf),
      (ffName "Data.String.CodeUnits" "drop", foreignFunction (op2 Text.drop)),
      (ffName "Data.String.CodeUnits" "length", foreignFunction (op1 Text.length)),
      notSupported (ffName "Data.String.Common" "_localeCompare"),
      (ffName "Data.String.Common" "replace", foreignFunction (op3 Text.replace)),
      (ffName "Data.String.Common" "split", foreignFunction (op2 Text.splitOn)),
      (ffName "Data.String.Common" "toLower", foreignFunction (op1 Text.toLower)),
      (ffName "Data.String.Common" "toUpper", foreignFunction (op1 Text.toUpper)),
      (ffName "Data.String.Common" "trim", foreignFunction (op1 Text.strip)),
      (ffName "Data.String.Common" "joinWith", foreignFunction (op2 Text.intercalate)),
      (ffName "Data.String.Unsafe" "charAt", foreignFunction (op2 (flip Text.index))),
      (ffName "Data.Unfoldable" "unfoldrArrayImpl", foreignFunction unfoldrArrayImpl),
      (ffName "Data.Unfoldable1" "unfoldr1ArrayImpl", foreignFunction unfoldr1ArrayImpl),
      (ffName "Data.Unit" "unit", foreignFunction unit),
      (ffName "Global" "infinity", foreignFunction (op0 (read "Infinity" :: Double))),
      (ffName "Global" "nan", foreignFunction (op0 (read "NaN" :: Double))),
      (ffName "Global" "isFinite", foreignFunction (op1 (not . isInfinite @Double))),
      (ffName "Global" "readFloat", foreignFunction (readAs Text.double)),
      (ffName "Global" "readInt", foreignFunction readInt),
      (ffName "Math" "floor", foreignFunction (op1 (fromIntegral @Int @Double . floor @Double @Int))),
      (ffName "Math" "remainder", foreignFunction (op2 (mod' @Double))),
      (ffName "Partial.Unsafe" "unsafePartial", foreignFunction unsafePartial),
      (ffName "Record.Unsafe" "unsafeGet", foreignFunction unsafeGet)
    ]
  where
    ffName mn n = QualifiedName (ModuleName <$> NonEmpty.fromList (Text.splitOn "." mn)) (Name n)
    notSupported :: QualifiedName -> (QualifiedName, SomeForeignFunction m)
    notSupported qn = (qn, SomeForeignFunction (NotSupported qn))
    indexImpl :: (Monad m, a ~ Value EvalAnn) => (a -> Ret m (Value EvalAnn)) -> Value EvalAnn -> Vector a -> Int -> Ret m (Value EvalAnn)
    indexImpl just nothing xs i = Ret (maybe (pure nothing) (unRet . just) (xs ^? ix (fromIntegral i)))
    sortImpl :: forall a m. (Monad m) => (a -> a -> Ret m Int) -> Vector a -> Ret m (Vector a)
    sortImpl comp xs = Vector.fromList <$> sortByM (\x y -> (`compare` 0) <$> comp x y) (Vector.toList xs)
    fromNumberImpl :: (Int -> Ret m (Value EvalAnn)) -> Value EvalAnn -> Double -> Ret m (Value EvalAnn)
    fromNumberImpl just _ = just . round
    fromStringAsImpl :: Monad m => (Int -> Ret m (Value EvalAnn)) -> Value EvalAnn -> Int -> Text -> Ret m (Value EvalAnn)
    fromStringAsImpl just nothing radix t =
      either (const (pure nothing)) (just . fst) $ case radix of
        10 -> Text.decimal t
        16 -> Text.hexadecimal t
        _ -> Left mempty
    lazyDefer :: Applicative m => (Value EvalAnn) -> Ret m (Value EvalAnn)
    lazyDefer = pure
    lazyForce :: (Value EvalAnn -> Ret m (Value EvalAnn)) -> Ret m (Value EvalAnn)
    lazyForce f = f (VObject mempty)
    len :: Monad m => Vector (Value EvalAnn) -> Ret m Int
    len xs = pure (fromIntegral (Vector.length xs))
    filterArray :: Monad m => (Value EvalAnn -> Ret m Bool) -> Vector (Value EvalAnn) -> Ret m (Vector (Value EvalAnn))
    filterArray f xs = Vector.filterM f xs
    arrayApply :: Applicative m => Vector (Value EvalAnn -> Ret m (Value EvalAnn)) -> Value EvalAnn -> Ret m (Vector (Value EvalAnn))
    arrayApply fs a = traverse ($ a) fs
    arrayUncons :: (() -> Ret m (Value EvalAnn)) -> (Value EvalAnn -> Vector (Value EvalAnn) -> Ret m (Value EvalAnn)) -> Vector (Value EvalAnn) -> Ret m (Value EvalAnn)
    arrayUncons empty' next xs = maybe (empty' ()) (uncurry next) (uncons xs)
    arrayRange :: Monad m => Int -> Int -> Ret m (Vector Int)
    arrayRange start end =
      let step = if start < end then 1 else (-1)
       in pure (Vector.enumFromStepN start step end)
    arrayBind :: forall a b m. Applicative m => Vector a -> (a -> Ret m (Vector b)) -> Ret m (Vector b)
    arrayBind xs f = join <$> traverse f xs
    arrayMap :: forall a b m. Monad m => (a -> Ret m b) -> Vector a -> Ret m (Vector b)
    arrayMap f xs = Vector.mapM f xs
    foldlArray :: forall a b m. Monad m => (b -> a -> Ret m b) -> b -> Vector a -> Ret m b
    foldlArray = foldM
    foldrArray :: forall a b m. Monad m => (a -> b -> Ret m b) -> b -> Vector a -> Ret m b
    foldrArray = foldrM
    arrayZipWith :: forall a b c m. Monad m => (a -> b -> Ret m c) -> Vector a -> Vector b -> Ret m (Vector c)
    arrayZipWith f = Vector.zipWithM f
    op0 :: forall a m. Monad m => a -> Ret m a
    op0 = pure
    op1 :: forall a b m. Monad m => (a -> b) -> a -> Ret m b
    op1 op = pure . op
    op2 :: forall a b c m. Monad m => (a -> b -> c) -> a -> b -> Ret m c
    op2 op x y = pure (op x y)
    op3 :: forall a b c d m. Monad m => (a -> b -> c -> d) -> a -> b -> c -> Ret m d
    op3 op x y z = pure (op x y z)
    readAs :: (Eval r m, StringConv s Text) => (Text -> Either s (a, Text)) -> Text -> Ret m a
    readAs parse t = Ret (either (throwError . ForeignFunctionError Nothing . toS) (pure . fst) (parse t))
    readInt :: Eval r m => Int -> Text -> Ret m Int
    readInt = \case
      10 -> readAs Text.decimal
      16 -> readAs Text.hexadecimal
      radix -> const (Ret $ throwError (ForeignFunctionError Nothing ("Unsupported radix for readInt: " <> show radix)))
    eqArray :: forall a b m. Monad m => (b ~ Bool) => (a -> a -> Ret m b) -> Vector a -> Vector a -> Ret m b
    eqArray pred' v1 v2
      | Vector.length v1 == Vector.length v2 = Vector.and <$> Vector.zipWithM pred' v1 v2
      | otherwise = pure False
    ordImpl :: forall a o m. Monad m => (Ord a) => o -> o -> o -> a -> a -> Ret m o
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
      (Value EvalAnn -> Ret m Bool) -> -- isNothing
      (Value EvalAnn -> Ret m (Value EvalAnn)) -> -- fromJust
      (Value EvalAnn -> Ret m (Value EvalAnn)) -> -- fst
      (Value EvalAnn -> Ret m (Value EvalAnn)) -> -- snd
      (Value EvalAnn -> Ret m (Value EvalAnn)) -> -- f
      Value EvalAnn -> -- b
      Ret m (Vector (Value EvalAnn))
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
    unfoldr1ArrayImpl ::
      Monad m =>
      (Value EvalAnn -> Ret m Bool) -> -- isNothing
      (Value EvalAnn -> Ret m (Value EvalAnn)) -> -- fromJust
      (Value EvalAnn -> Ret m (Value EvalAnn)) -> -- fst
      (Value EvalAnn -> Ret m (Value EvalAnn)) -> -- snd
      (Value EvalAnn -> Ret m (Value EvalAnn)) -> -- f
      Value EvalAnn -> -- b
      Ret m (Vector (Value EvalAnn))
    unfoldr1ArrayImpl isNothing' fromJust' fst' snd' f initial =
      flip Vector.unfoldrM (Just initial) $ \case
        Nothing -> pure Nothing
        Just b -> do
          tuple <- f b
          a <- fst' tuple
          b' <- snd' tuple
          isNothing' b' >>= \case
            True -> pure (Just (a, Nothing))
            False -> do
              seed <- fromJust' b'
              pure (Just (a, Just seed))
    unsafePartial :: Eval r m => Value EvalAnn -> Ret m (Value EvalAnn)
    unsafePartial f = Ret $ do
      env' <- view (field @"env")
      Function fenv _ body <- require P.nullSourceSpan (Proxy @"VFunction") f
      local (field @"env" .~ fenv {envForeignFunctions = envForeignFunctions env'}) (eval body)
    unsafeGet :: MonadError EvalError m => Text -> HashMap Text (Value EvalAnn) -> Ret m (Value EvalAnn)
    unsafeGet k xs = Ret (accessField P.nullSourceSpan k xs)
    toCodePointArray :: Monad m => Value EvalAnn -> Value EvalAnn -> Text -> Ret m (Vector Int)
    toCodePointArray _ _ t = pure (Vector.map ord (Vector.fromList (toS t)))
    unsafeCodePointAt0 :: Monad m => Value EvalAnn -> Text -> Ret m Int
    unsafeCodePointAt0 _ t = pure (ord (Text.index t 0))
    unit :: Monad m => Ret m (Value EvalAnn)
    unit = pure (VObject mempty)
    codeUnitsIndexOf :: Applicative m => (Int -> Ret m (Value EvalAnn)) -> Value EvalAnn -> Text -> Text -> Ret m (Value EvalAnn)
    codeUnitsIndexOf just nothing x s = maybe (pure nothing) just (headMay (Text.indices x s))
