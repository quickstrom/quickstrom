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

module WebCheck.PureScript.Queries where

import Control.Lens hiding (op)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Morph (MFunctor (hoist))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import qualified Data.Aeson as JSON
import Data.Generics.Product (HasField, field)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Scientific (floatingOrInteger)
import qualified Data.Vector as Vector
import qualified Language.PureScript.CoreFn as CF
import Protolude hiding (Selector)
import WebCheck.Element (Selector (..))
import WebCheck.PureScript.Eval
import WebCheck.PureScript.Eval.Ann
import WebCheck.PureScript.Eval.Class
import WebCheck.PureScript.Eval.Env
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Value
import WebCheck.Specification (Queries)
import WebCheck.Trace (ObservedState (..))

newtype ExtractEnv = ExtractEnv {env :: Env Extract EvalAnn}
  deriving (Generic)

newtype Extract a = Extract (ReaderT ExtractEnv (WriterT Queries (Except EvalError)) a)
  deriving (Functor, Applicative, Monad, MonadError EvalError, MonadWriter Queries, MonadReader ExtractEnv, MonadFix)

runExtract :: Env Extract EvalAnn -> Extract a -> Either EvalError Queries
runExtract env' (Extract ma) = runExcept (execWriterT (runReaderT ma (ExtractEnv env')))

instance MonadEvalQuery Extract where

  -- TODO
  evalQuery p1 p2 = do
    selector <- require (exprSourceSpan p1) (Proxy @"VString") =<< eval p1
    wantedStates <-
      traverse (require (exprSourceSpan p2) (Proxy @"VElementState")) . HashMap.elems
        =<< require (exprSourceSpan p2) (Proxy @"VObject")
        =<< eval p2
    tell (HashMap.singleton (Selector selector) (HashSet.fromList wantedStates))
    pure (VArray mempty)

  evalNext = eval

  evalAlways = eval

data WithObservedStatesEnv
  = WithObservedStatesEnv
      { env :: Env WithObservedStates EvalAnn,
        observedStates :: [ObservedState]
      }
  deriving (Generic)

newtype WithObservedStates a = WithObservedStates (ReaderT WithObservedStatesEnv (Except EvalError) a)
  deriving (Functor, Applicative, Monad, MonadError EvalError, MonadReader WithObservedStatesEnv, MonadFix)

runWithObservedStates :: Env WithObservedStates EvalAnn -> [ObservedState] -> WithObservedStates a -> Either EvalError a
runWithObservedStates env' observedStates' (WithObservedStates ma) =
  runExcept (runReaderT ma (WithObservedStatesEnv env' observedStates'))

instance MonadEvalQuery WithObservedStates where

  evalQuery p1 p2 = do
    view (field @"observedStates") >>= \case
      [] -> throwError Undetermined
      ObservedState current : _ -> do
        selector <- require (exprSourceSpan p1) (Proxy @"VString") =<< eval p1
        wantedStates <- require (exprSourceSpan p2) (Proxy @"VObject") =<< eval p2
        matchedElements <-
          maybe
            (throwError (ForeignFunctionError (Just (exprSourceSpan p1)) ("Selector not in observed state: " <> selector)))
            pure
            (HashMap.lookup (Selector selector) current)
        mappedElements <- for (Vector.fromList matchedElements) $ \matchedElement -> do
          mappings <- flip HashMap.traverseWithKey wantedStates $ \k s -> do
            elementState <- require (exprSourceSpan p2) (Proxy @"VElementState") s
            case HashMap.lookup elementState matchedElement of
              Just x -> pure (fromValue x)
              Nothing ->
                let msg = ("Element state (bound to ." <> k <> ") not in observed state for query `" <> selector <> "`: " <> show elementState)
                 in throwError (ForeignFunctionError (Just (exprSourceSpan p2)) msg)
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

  evalNext p =
    view (field @"observedStates") >>= \case
      [] -> throwError Undetermined
      _ : rest -> local (field @"observedStates" .~ rest) (eval p)

  evalAlways p =
    view (field @"observedStates") >>= \case
      [] -> pure (VBool True)
      _ -> do
        first' <- require (exprSourceSpan p) (Proxy @"VBool")
          =<< eval p `catchError` \case
            Undetermined -> pure (VBool True)
            e -> throwError e
        rest' <-
          require (exprSourceSpan p) (Proxy @"VBool")
            =<< local (field @"observedStates" %~ drop 1) (eval p)
        pure (VBool (first' && rest'))
