{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module WebCheck.PureScript.Eval.WithObservedStates where

import Control.Lens hiding (op)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific (floatingOrInteger)
import qualified Data.Vector as Vector
import WebCheck.Element (Selector (..))
import WebCheck.Prelude
import WebCheck.PureScript.Eval.Class
import WebCheck.PureScript.Eval.Error
import WebCheck.PureScript.Eval.Interpret
import WebCheck.PureScript.Value
import WebCheck.Trace (ObservedState (..))

data WithObservedStatesEnv
  = WithObservedStatesEnv
      { env :: Env' WithObservedStates,
        observedStates :: [ObservedState]
      }
  deriving (Generic)

newtype WithObservedStates a = WithObservedStates (ReaderT WithObservedStatesEnv (Except EvalError) a)
  deriving (Functor, Applicative, Monad, MonadError EvalError, MonadReader WithObservedStatesEnv, MonadFix)

runWithObservedStates :: Env' WithObservedStates -> [ObservedState] -> WithObservedStates a -> Either EvalError a
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

  evalNext _ p =
    view (field @"observedStates") >>= \case
      (_ : rest) | not (null rest) -> local (field @"observedStates" .~ rest) (eval p)
      _ -> throwError Undetermined

  evalAlways ann p =
    view (field @"observedStates") >>= \case
      [] -> pure (VBool True)
      _ -> do
        first' <- require (exprSourceSpan p) (Proxy @"VBool") =<< eval p `catchError` \case
          Undetermined -> pure (VBool True)
          e -> throwError e
        rest' <-
          require (exprSourceSpan p) (Proxy @"VBool")
            =<< local (field @"observedStates" %~ drop 1) (evalAlways ann p)
        pure (VBool (first' && rest'))
