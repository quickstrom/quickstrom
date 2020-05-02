{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    ObservedState (..),
    Trace (..),
    ActionResult (..),
    TraceElement (..),
    traceElements,
    observedStates,
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
import Data.Hashable (Hashable)
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
import WTP.Formula.Logic
import qualified WTP.Type as WTP
import WTP.Specification (Action (..), Path (..), Selected (..))
import Prelude hiding (Bool (..), not)
import qualified Data.Set as Set

data ElementStateValue where
  ElementStateValue :: forall (a :: WTP.Type). Typeable a => ElementState a -> FValue a -> ElementStateValue

deriving instance Show ElementStateValue

instance Eq ElementStateValue where
  ElementStateValue (s1 :: ElementState t1) (v1 :: FValue t1) == ElementStateValue (s2 :: ElementState t2) (v2 :: FValue t2) =
    case eqTypeRep (typeRep @t1) (typeRep @t2) of
      Just HRefl -> s1 == s2 && v1 == v2
      Nothing -> Bool.False

findElementState :: Typeable a => ElementState a -> [ElementStateValue] -> Maybe (FValue a)
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

runQueryInState :: ObservedState -> Formula a -> Formula a
runQueryInState ObservedState {queriedElements, elementStates} =
  id
  {-
  run
    . interpret
      ( \case
          Query selector -> case HashMap.lookup selector queriedElements of
            Just (a : _) -> pure (Just a)
            _ -> pure Nothing
          Get state element' ->
            let states = fromMaybe mempty (HashMap.lookup element' elementStates)
             in pure (fromJust (findElementState state states))
      )
      -}

newtype Trace ann = Trace [TraceElement ann]
  deriving (Show, Generic)

traceElements :: Monoid r => Getting r (Trace ann) [TraceElement ann]
traceElements = position @1

observedStates :: Monoid r => Getting r (Trace ann) ObservedState
observedStates = traceElements . traverse . _Ctor @"TraceState" . position @2

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

annotateStutteringSteps :: Trace a -> Trace TraceElementEffect
annotateStutteringSteps (Trace els) = Trace (go els mempty)
  where
    -- TODO: Not tail-recursive, might need optimization
    go (TraceAction _ action ActionSuccess : TraceState _ newState : rest) lastState =
      let ann' = if newState == lastState then Stutter else NoStutter
       in (TraceAction ann' action ActionSuccess : TraceState ann' newState : go rest newState)
    go (el : rest) lastState = (el & ann .~ NoStutter) : go rest lastState
    go [] _ = []

prettyValue :: FValue a -> Doc AnsiStyle
prettyValue = \case
  VTrue -> "true"
  VFalse -> "false"
  VString t -> pretty (show t)
  VElement e -> "element" <+> parens (pretty e)
  VSet vs -> encloseSep lbrace rbrace comma (map prettyValue (Set.toList vs))
  VSeq vs -> encloseSep lparen rparen comma (map prettyValue vs)


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
              ActionFailed{} -> effect `stutterColorOr` Red <> bold
              ActionImpossible -> color Yellow <> bold
         in annotate annotation (pretty i <> "." <+> prettyAction action)
      TraceState effect state -> annotate (effect `stutterColorOr` Blue <> bold) (pretty i <> "." <+> "State") <> line <> indent 2 (prettyObservedState state)
    Stutter `stutterColorOr` _ = colorDull Black
    NoStutter `stutterColorOr` fallback = color fallback

prettyObservedState :: ObservedState -> Doc AnsiStyle
prettyObservedState state =
  vsep
    [ annotate bold "Queried elements:",
      indent
        2
        ( vsep
            ( withValues
                queriedElements
                ( \(sel, el) ->
                    bullet <+> align (pretty sel <> line <> indent 2 (vsep (map pretty el)))
                )
            )
        ),
      annotate bold "Element states:",
      indent
        2
        ( vsep
            ( withValues
                elementStates
                ( \(el, states) ->
                    bullet <+> align (pretty el <> line <> indent 2 (vsep (map prettyElementStateValue states)))
                )
            )
        )
    ]
  where
    withValues :: (Ord k, Eq k, Hashable k) => (ObservedState -> HashMap k v) -> ((k, v) -> s) -> [s]
    withValues field f =
      field state
        & HashMap.toList
        & List.sortBy (comparing fst)
        & map f
    prettyElementStateValue :: ElementStateValue -> Doc AnsiStyle
    prettyElementStateValue = \case
      -- (ElementStateValue (Attribute name) (Left b)) -> "attribute" <+> pretty name <> "=" <> if b then "true" else "false"
      (ElementStateValue (Attribute name) value) -> "attribute" <+> pretty name <> "=" <> prettyValue value
      -- (ElementStateValue (Property name) value) -> "property" <+> pretty name <+> "=" <+> pretty (JSON.encodeToLazyText value)
      (ElementStateValue (CssValue name) (VString t)) -> "css" <+> braces (pretty name <> ":" <+> pretty t <> ";")
      (ElementStateValue Text value) -> "text" <> "=" <> prettyValue value
      (ElementStateValue Enabled b) -> case b of VTrue -> "enabled"; VFalse -> "disabled"
