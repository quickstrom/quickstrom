{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

module WebCheck.PureScript.Analyze where

import Control.Lens hiding (op)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Protolude hiding (Selector)
import WebCheck.Element (Selector (..))
import WebCheck.PureScript.Eval.Interpret
import WebCheck.PureScript.Eval.Class
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Value
import WebCheck.Specification (Queries)

newtype ExtractEnv = ExtractEnv {env :: Env' Extract}
  deriving (Generic)

newtype Extract a = Extract (ReaderT ExtractEnv (WriterT Queries (Except EvalError)) a)
  deriving (Functor, Applicative, Monad, MonadError EvalError, MonadWriter Queries, MonadReader ExtractEnv, MonadFix)

runExtract :: Env' Extract -> Extract a -> Either EvalError Queries
runExtract env' (Extract ma) = runExcept (execWriterT (runReaderT ma (ExtractEnv env')))

instance MonadEvalQuery Extract where
  evalQuery p1 p2 = do
    selector <- require (exprSourceSpan p1) (Proxy @"VString") =<< eval p1
    wantedStates <-
      traverse (require (exprSourceSpan p2) (Proxy @"VElementState")) . HashMap.elems
        =<< require (exprSourceSpan p2) (Proxy @"VObject")
        =<< eval p2
    tell (HashMap.singleton (Selector selector) (HashSet.fromList wantedStates))
    pure (VArray mempty)

  evalNext _ = eval
  evalAlways _ = eval
