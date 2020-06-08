{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module WebCheck.Trace
  ( Selected (..),
    Action (..),
    ObservedState (..),
    Trace (..),
    ActionResult (..),
    TraceElement (..),
    traceElements,
    observedStates,
    traceActions,
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
import WebCheck.Action
import WebCheck.Element
import Prelude hiding (Bool (..), not)

newtype ObservedState = ObservedState (HashMap Selector [HashMap ElementState Value])
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Semigroup ObservedState where
  ObservedState s1 <> ObservedState s2 = ObservedState (s1 <> s2)

instance Monoid ObservedState where
  mempty = ObservedState mempty

newtype Trace ann = Trace [TraceElement ann]
  deriving (Show, Generic)

traceElements :: Lens' (Trace ann) [TraceElement ann]
traceElements = position @1

observedStates :: Traversal' (Trace ann) ObservedState
observedStates = traceElements . traverse . _Ctor @"TraceState" . position @2

traceActions :: Traversal' (Trace ann) (Action Selected)
traceActions = traceElements . traverse . _Ctor @"TraceAction" . position @2

nonStutterStates :: Monoid r => Getting r (Trace TraceElementEffect) ObservedState
nonStutterStates = traceElements . traverse . _Ctor @"TraceState" . filtered ((== NoStutter) . fst) . position @2

data ActionResult = ActionSuccess | ActionFailed Text | ActionImpossible
  deriving (Show, Generic)

data TraceElement ann
  = TraceAction ann (Action Selected) ActionResult
  | TraceState ann ObservedState
  -- TODO: `TraceEvent` when queried DOM nodes change
  deriving (Show, Generic)

ann :: Lens (TraceElement ann) (TraceElement ann2) ann ann2
ann = position @1

data TraceElementEffect = Stutter | NoStutter
  deriving (Show, Eq)

annotateStutteringSteps :: Trace a -> Trace TraceElementEffect
annotateStutteringSteps (Trace els) = Trace (go els mempty)
  where
    -- TODO: Not tail-recursive, might neeu optimization
    go (TraceAction _ action result : rest) lastState =
      (TraceAction NoStutter action result : go rest lastState)
    go (TraceState _ newState : rest) lastState =
      let ann' = if newState == lastState then Stutter else NoStutter
       in TraceState ann' newState : go rest newState
    go [] _ = []

withoutStutterStates :: Trace TraceElementEffect -> Trace TraceElementEffect
withoutStutterStates t = Trace (t ^.. traceElements . folded . filtered ((== NoStutter) . view ann))
