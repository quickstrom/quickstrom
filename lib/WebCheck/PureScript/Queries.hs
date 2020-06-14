{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module WebCheck.PureScript.Queries where

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Writer (MonadWriter, tell, WriterT, execWriterT)
import qualified Language.PureScript.CoreFn as CF
import Protolude hiding (Selector)
import WebCheck.Element (Selector(..))
import WebCheck.PureScript.Eval
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Value
import WebCheck.Specification (Queries)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Fix (MonadFix)

newtype Extract m a = Extract (WriterT Queries m a)
  deriving (Functor, Applicative, Monad, MonadError e, MonadWriter Queries, MonadTrans, MFunctor, MonadFix)

runExtract :: Monad m => Extract m a -> m Queries
runExtract (Extract ma) = execWriterT ma

instance (MonadEval m, MonadFix m) => MonadEval (Extract m) where

  modifyEnv f = hoist (modifyEnv f)

  eval = \case
    CF.App _ (BuiltIn "_queryAll" _ p) q -> do
      selector <- require (exprSourceSpan p) (Proxy @"VString") =<< eval p
      wantedStates <- 
        (pure . HashSet.fromList . HashMap.elems)
        =<< traverse (require (exprSourceSpan q) (Proxy @"VElementState"))
        =<< require (exprSourceSpan q) (Proxy @"VObject")
        =<< eval q
      tell (HashMap.singleton (Selector selector) wantedStates)
      pure (VArray mempty)
    expr -> lift (eval expr)