{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Quickstrom.PureScript.Eval.WithObservedStates where

import Control.Lens hiding (op)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific (floatingOrInteger)
import qualified Data.Vector as Vector
import Quickstrom.Element (ElementState (..), Selector (..))
import Quickstrom.Prelude
import Quickstrom.PureScript.Eval.Class
import Quickstrom.PureScript.Eval.Error
import Quickstrom.PureScript.Eval.Interpret
import Quickstrom.PureScript.Value
import Quickstrom.Trace (ObservedState (..))

data WithObservedStatesEnv = WithObservedStatesEnv
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
              Just json -> case parseValueAs elementState json of
                Just value -> pure value
                Nothing ->
                  let msg = ("Value (bound to ." <> k <> ") could not be parsed as `" <> show elementState <> "`: " <> show json)
                   in throwError (ForeignFunctionError (Just (exprSourceSpan p2)) msg)
              Nothing ->
                let msg = ("Element state (bound to ." <> k <> ") not in observed state for query `" <> selector <> "`: " <> show elementState)
                 in throwError (ForeignFunctionError (Just (exprSourceSpan p2)) msg)
          pure (VObject mappings)
        pure (VArray mappedElements)
        where
          parseValueAs :: ElementState -> JSON.Value -> Maybe (Value ann)
          parseValueAs Attribute {} = \case
            JSON.Null -> pure (VObject [("constructor", VString "Nothing"), ("fields", VArray mempty)])
            JSON.String t -> pure (VObject [("constructor", VString "Just"), ("fields", VArray [VString t])])
            _ -> Nothing
          parseValueAs Property {} = fromJson
          parseValueAs CssValue {} = fromJson
          parseValueAs Text {} = fromJson
          fromJson :: JSON.Value -> Maybe (Value ann)
          fromJson = \case
            JSON.Null -> Nothing
            JSON.String t -> pure (VString t)
            JSON.Bool b -> pure (VBool b)
            JSON.Number n -> pure (either VNumber VInt (floatingOrInteger n))
            JSON.Array xs -> VArray <$> traverse fromJson xs
            JSON.Object xs -> VObject <$> traverse fromJson xs

  evalNext _ p =
    view (field @"observedStates") >>= \case
      (_ : rest) | not (null rest) -> local (field @"observedStates" .~ rest) (eval p)
      _ -> throwError Undetermined

  evalAlways ann p =
    view (field @"observedStates") >>= \case
      [] -> pure (VBool True)
      _ -> do
        first' <-
          require (exprSourceSpan p) (Proxy @"VBool") =<< eval p `catchError` \case
            Undetermined -> pure (VBool True)
            e -> throwError e
        rest' <-
          require (exprSourceSpan p) (Proxy @"VBool")
            =<< local (field @"observedStates" %~ drop 1) (evalAlways ann p)
        pure (VBool (first' && rest'))

  evalUntil ann p q =
    view (field @"observedStates") >>= \case
      [] -> pure (VBool False)
      _ -> do
        doesQHold <-
          require (exprSourceSpan p) (Proxy @"VBool") =<< eval q `catchError` \case
            Undetermined -> pure (VBool False)
            e -> throwError e
        if doesQHold
            then pure (VBool True)
            else do
                doesPHold <-
                  require (exprSourceSpan p) (Proxy @"VBool") =<< eval p `catchError` \case
                    Undetermined -> pure (VBool False)
                    e -> throwError e
                rest' <-
                  require (exprSourceSpan p) (Proxy @"VBool")
                    =<< local (field @"observedStates" %~ drop 1) (evalUntil ann p q)
                pure (VBool (doesPHold && rest'))
