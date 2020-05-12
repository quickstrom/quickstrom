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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module WTP.Trace
  ( ElementStateValue (..),
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
import WTP.Specification (Action (..), Path (..), Selected (..))
import Prelude hiding (Bool (..), not)
import qualified Data.Aeson.Text as JSON

data ElementStateValue where
  ElementStateValue :: forall a. (Show a, Typeable a, Eq a) => ElementState a -> a -> ElementStateValue

deriving instance Show ElementStateValue

instance Eq ElementStateValue where
  ElementStateValue (s1 :: ElementState t1) (v1 :: t1) == ElementStateValue (s2 :: ElementState t2) (v2 :: t2) =
    case eqTypeRep (typeRep @t1) (typeRep @t2) of
      Just HRefl -> s1 == s2 && v1 == v2
      Nothing -> Bool.False

findElementState :: Typeable a => ElementState a -> [ElementStateValue] -> Maybe a
findElementState _ [] = Nothing
findElementState (state :: ElementState s1) (ElementStateValue (_ :: ElementState s2) value : rest) =
  case eqTypeRep (typeRep @s1) (typeRep @s2) of
    Just HRefl -> Just value
    Nothing -> findElementState state rest

data ObservedState
  = ObservedState
      { queriedElements :: HashMap Selector [Element],
        elementStates :: HashMap Element [ElementStateValue]
      }
  deriving (Show, Eq, Generic)

instance Semigroup ObservedState where
  ObservedState q1 s1 <> ObservedState q2 s2 = ObservedState (q1 <> q2) (s1 <> s2)

instance Monoid ObservedState where
  mempty = ObservedState mempty mempty

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
prettyObservedState state =
  vsep
    ( withValues
        queriedElements
        ( \(sel, el) ->
            bullet <+> align (pretty sel <> line <> indent 2 (vsep (map prettyElementState el)))
        )
    )
  where
    withValues :: (Ord k) => (ObservedState -> HashMap k v) -> ((k, v) -> s) -> [s]
    withValues field f =
      field state
        & HashMap.toList
        & List.sortBy (comparing fst)
        & map f
    prettyElementState :: Element -> Doc AnsiStyle
    prettyElementState el =
      pretty el <> line <> indent 2 (vsep (map prettyElementStateValue (HashMap.lookupDefault mempty el (elementStates state))))
    prettyElementStateValue :: ElementStateValue -> Doc AnsiStyle
    prettyElementStateValue = \case
      (ElementStateValue (Attribute name) value) -> "attribute" <+> pretty name <> "=" <> pretty (show value)
      (ElementStateValue (Property name) value) -> "property" <+> pretty name <> "=" <> pretty (JSON.encodeToLazyText value)
      (ElementStateValue (CssValue name) t) -> "css" <+> braces (pretty name <> ":" <+> pretty t <> ";")
      (ElementStateValue Text value) -> "text" <> "=" <> pretty (show value)
      (ElementStateValue Enabled b) -> if b then "enabled" else "disabled"
