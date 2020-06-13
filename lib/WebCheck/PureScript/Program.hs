{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module WebCheck.PureScript.Program where

import Protolude hiding (moduleName)
import Control.Lens hiding (op)
import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Generics.Product (field)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import Language.PureScript.CoreFn hiding (Ann)
import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.Names
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import WebCheck.PureScript.Value

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

data Program ann
  = Program
      { programLibraryModules :: Modules,
        programMain :: Module CF.Ann,
        programEnv :: Env ann
      }
  deriving (Show)

programQualifiedName :: Text -> Program ann -> Qualified Ident
programQualifiedName name p =
  Qualified (Just (moduleName (programMain p))) (Ident name)
