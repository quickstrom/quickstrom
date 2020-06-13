{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module WebCheck.PureScript.Queries where

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Writer (WriterT, execWriterT)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Protolude hiding (Selector)
import WebCheck.Element (ElementState, Selector)
import WebCheck.PureScript.Eval.Class
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Value
import WebCheck.PureScript.Program
import WebCheck.Specification (Queries)
import qualified Language.PureScript as P

newtype Extract m a = Extract (WriterT Queries m a)
  deriving (Functor, Applicative, Monad, MonadError e, MonadTrans)

runExtract :: Monad m => Extract m a -> m Queries
runExtract (Extract ma) = execWriterT ma

instance MonadEval m => MonadEval (Extract m) where

  type Ann (Extract m) = Ann m

  eval = lift . eval

  evalFunc f = lift . evalFunc f

extractQueries :: MonadEval m => Program (Ann m) -> m (HashMap Selector (HashSet ElementState))
extractQueries prog = runExtract $ do
  let qn = programQualifiedName "proposition" prog
  case envLookup qn (programEnv prog) of
      Just (Left expr) -> eval expr
      Just (Right val) -> pure val
      Nothing -> throwError (NotInScope P.nullSourceSpan qn)
