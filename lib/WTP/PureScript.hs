{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module WTP.PureScript where

import           Language.PureScript.AST.Literals
import           Language.PureScript.Comments
import qualified Language.PureScript.Constants    as PureScript
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Language.PureScript.PSString

import System.FilePath.Glob (glob)
import "purescript" Control.Monad.Logger (runLogger')
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Data.Text (Text)
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Writer.Strict (WriterT(runWriterT))
import WTP.Value

data EvalError

eval :: Module -> Ident -> Either EvalError Module
eval inputGlobs input = runExceptT $ do
  inputFiles <- liftIO (concat <$> traverse glob inputGlobs)
  mods :: [(FilePath, Module)] <- ExceptT $ I.loadAllModules inputFiles
  lift (print (head mods))
  case CST.parseModuleFromFile "<file>" input >>= CST.resFull of
    Left parseError ->
      throwError $ CST.toMultipleErrors "<file>" parseError
    Right m -> do
      flip runReaderT P.defaultOptions $ do
        ((P.Module ss coms moduleName elaborated exps, _env), _) <- fmap fst . runWriterT . P.runSupplyT 0 $ do
          desugared <- (P.desugar P.primEnv mempty [P.importPrim m]) >>= \case
            [d] -> pure d
            _ -> error "desugaring did not produce one module"
          P.runCheck' (P.emptyCheckState P.initEnvironment) $ P.typeCheckModule desugared
        regrouped <- P.createBindingGroups moduleName . P.collapseBindingGroups $ elaborated
        pure (P.Module ss coms moduleName regrouped exps)
