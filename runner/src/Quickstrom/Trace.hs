{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Quickstrom.Trace
  ( Selected (..),
    Action (..),
    ObservedElementStates (..),
    ObservedState (..),
    Trace (..),
    ActionResult (..),
    TraceElement (..),
    traceElements,
    observedStates,
    traceActions,
    traceActionFailures,
    nonStutterStates,
    TraceElementEffect (..),
    annotateStutteringSteps,
    withoutStutterStates,
  )
where

import Control.Lens
import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Generics.Product (position)
import Data.Generics.Sum (_Ctor)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Quickstrom.Action
import Quickstrom.Element
import Quickstrom.Prelude
import Prelude hiding (Bool (..), not)

newtype ObservedElementStates = ObservedElementStates (HashMap Selector [HashMap ElementState Value])
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Semigroup ObservedElementStates where
  ObservedElementStates s1 <> ObservedElementStates s2 = ObservedElementStates (s1 <> s2)

instance Monoid ObservedElementStates where
  mempty = ObservedElementStates mempty

data ObservedState = ObservedState {screenshot :: Maybe ByteString, elementStates :: ObservedElementStates}
  deriving (Show, Generic)

instance Semigroup ObservedState where
  ObservedState s1 e1 <> ObservedState s2 e2 = ObservedState (s1 <> s2) (e1 <> e2)

instance Monoid ObservedState where
  mempty = ObservedState mempty mempty

newtype Trace ann = Trace [TraceElement ann]
  deriving (Show, Generic)

data ActionResult = ActionSuccess | ActionFailed Text | ActionImpossible
  deriving (Show, Generic)

data TraceElement ann
  = TraceAction ann (Action Selected) ActionResult
  | TraceState ann ObservedState
  deriving (Show, Generic)

traceElements :: Lens' (Trace ann) [TraceElement ann]
traceElements = position @1

observedStates :: Traversal' (Trace ann) ObservedState
observedStates = traceElements . traverse . _Ctor @"TraceState" . position @2

traceActions :: Traversal' (Trace ann) (Action Selected)
traceActions = traceElements . traverse . _Ctor @"TraceAction" . position @2

traceActionFailures :: Traversal' (Trace ann) Text
traceActionFailures = traceElements . traverse . _Ctor @"TraceAction" . position @3 . _Ctor @"ActionFailed"

nonStutterStates :: Monoid r => Getting r (Trace TraceElementEffect) ObservedState
nonStutterStates = traceElements . traverse . _Ctor @"TraceState" . filtered ((== NoStutter) . fst) . position @2

ann :: Lens (TraceElement ann) (TraceElement ann2) ann ann2
ann = position @1

data TraceElementEffect = Stutter | NoStutter
  deriving (Show, Eq, Generic, ToJSON)

annotateStutteringSteps :: Trace a -> Trace TraceElementEffect
annotateStutteringSteps (Trace els) = Trace (go els mempty)
  where
    -- TODO: Not tail-recursive, might need optimization
    go (TraceAction _ action result : rest) lastState =
      (TraceAction NoStutter action result : go rest lastState)
    go (TraceState _ newState : rest) lastState =
      let ann' = if elementStates newState == elementStates lastState then Stutter else NoStutter
       in TraceState ann' newState : go rest newState
    go [] _ = []

withoutStutterStates :: Trace TraceElementEffect -> Trace TraceElementEffect
withoutStutterStates t = Trace (t ^.. traceElements . folded . filtered ((== NoStutter) . view ann))
