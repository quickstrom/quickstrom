{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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

module WTP.Trace
  ( SomeValue (..),
    castValue,
    findElementState,
    ObservedState (..),
    Trace (..),
    ActionResult (..),
    TraceElement (..),
    traceElements,
    observedStates,
    traceActions,
    nonStutterStates,
    TraceElementEffect,
    annotateStutteringSteps,
    prettyAction,
    prettyActions,
    prettyTrace,
    prettySelected,
  )
where

import Control.Lens
import qualified Data.Bool as Bool
import Data.Function ((&))
import Data.Generics.Product (position)
import Data.Generics.Sum (_Ctor)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Symbols.Unicode (bullet)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Type.Reflection
import WTP.Element
import WTP.Query
import WTP.Specification (Action (..), Path (..), Selected (..))
import Prelude hiding (Bool (..), not)

data SomeValue where
  SomeValue :: forall a. (Show a, Typeable a, Eq a) => a -> SomeValue

deriving instance Show SomeValue

instance Eq SomeValue where
  SomeValue (v1 :: t1) == SomeValue (v2 :: t2) =
    case eqTypeRep (typeRep @t1) (typeRep @t2) of
      Just HRefl -> v1 == v2
      Nothing -> Bool.False

castValue :: TypeRep a -> SomeValue -> Maybe a
castValue rep (SomeValue  (v :: t)) =
  case eqTypeRep (typeRep @t) rep of
    Just HRefl -> Just v
    Nothing -> Nothing

findElementState :: Typeable a => ElementState a -> [SomeValue] -> Maybe a
findElementState _ [] = Nothing
findElementState (state :: ElementState s1) (SomeValue (value :: s2) : rest) =
  case eqTypeRep (typeRep @s1) (typeRep @s2) of
    Just HRefl -> Just value
    Nothing -> findElementState state rest

newtype ObservedState = ObservedState (HashMap SomeQuery [SomeValue])
  deriving (Show, Eq, Generic)

instance Semigroup ObservedState where
  ObservedState s1 <> ObservedState s2 = ObservedState (s1 <> s2)

instance Monoid ObservedState where
  mempty = ObservedState mempty 

newtype Trace ann = Trace [TraceElement ann]
  deriving (Show, Generic)

traceElements :: Getting r (Trace ann) [TraceElement ann]
traceElements = position @1

observedStates :: Monoid r => Getting r (Trace ann) ObservedState
observedStates = traceElements . traverse . _Ctor @"TraceState" . position @2

traceActions :: Monoid r => Getting r (Trace ann) (Action Selected)
traceActions = traceElements . traverse . _Ctor @"TraceAction" . position @2

nonStutterStates :: Monoid r => Getting r (Trace TraceElementEffect) ObservedState
nonStutterStates = traceElements . traverse . _Ctor @"TraceState" . filtered ((== NoStutter).fst) . position @2

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
    -- TODO: Not tail-recursive, might need optimization
    go (TraceAction _ action ActionSuccess : TraceState _ newState : rest) lastState =
      let ann' = if newState == lastState then Stutter else NoStutter
       in (TraceAction ann' action ActionSuccess : TraceState ann' newState : go rest newState)
    go (el : rest) lastState = (el & ann .~ NoStutter) : go rest lastState
    go [] _ = []

prettyAction :: Action Selected -> Doc AnsiStyle
prettyAction = \case
  Click sel -> "click" <+> prettySelected sel
  Focus sel -> "focus" <+> prettySelected sel
  KeyPress key -> "key press" <+> pretty (show key)
  Navigate (Path path) -> "navigate to" <+> pretty path
  Wait (min', max') -> "wait" <+> pretty min' <> "ms-" <> pretty max' <> "ms"

prettySelected :: Selected -> Doc AnsiStyle
prettySelected (Selected (Selector sel) i) = pretty sel <> brackets (pretty i)

prettyActions :: [Action Selected] -> Doc AnsiStyle
prettyActions actions = vsep (zipWith item [1 ..] actions)
  where
    item :: Int -> Action Selected -> Doc AnsiStyle
    item i = \case
      action -> (pretty i <> "." <+> prettyAction action)

prettyTrace :: Trace TraceElementEffect -> Doc AnsiStyle
prettyTrace (Trace elements') = vsep (zipWith prettyElement [1 ..] elements')
  where
    prettyElement :: Int -> TraceElement TraceElementEffect -> Doc AnsiStyle
    prettyElement i = \case
      TraceAction effect action result ->
        let annotation = case result of
              ActionSuccess -> effect `stutterColorOr` Blue <> bold
              ActionFailed {} -> effect `stutterColorOr` Red <> bold
              ActionImpossible -> color Yellow <> bold
         in annotate annotation (pretty i <> "." <+> prettyAction action)
      TraceState effect state -> annotate (effect `stutterColorOr` Blue <> bold) (pretty i <> "." <+> "State") <> line <> indent 2 (prettyObservedState state)
    Stutter `stutterColorOr` _ = colorDull Black
    NoStutter `stutterColorOr` fallback = color fallback

prettyObservedState :: ObservedState -> Doc AnsiStyle
prettyObservedState (ObservedState state) =
  vsep
    ( withValues
        ( \(SomeQuery query, values) ->
            bullet <+> align (prettyQuery query <> line <> indent 2 (vsep (map prettySomeValue values)))
        )
    )
  where
    withValues :: ((SomeQuery, [SomeValue]) -> s) -> [s]
    withValues f =
      state
        & HashMap.toList
        & List.sortBy (comparing fst)
        & map f

    prettyQuery :: Query a -> Doc AnsiStyle
    prettyQuery = \case
      _ -> mempty
    prettySomeValue :: SomeValue -> Doc AnsiStyle
    prettySomeValue (SomeValue value) = pretty (show value)
