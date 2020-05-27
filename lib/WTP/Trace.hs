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

module WTP.Trace
  ( ObservedState (..),
    Trace (..),
    ActionResult (..),
    TraceElement (..),
    traceElements,
    observedStates,
    traceActions,
    nonStutterStates,
    TraceElementEffect,
    annotateStutteringSteps,
    withoutStutterStates,
    prettyAction,
    prettyActions,
    prettyTrace,
    prettyQuery,
    prettyValue,
    prettySelected,
  )
where

import Control.Lens
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Function ((&))
import Data.Generics.Product (position)
import Data.Generics.Sum (_Ctor)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Symbols.Unicode (bullet)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import WTP.Element
import WTP.Query
import WTP.Specification (Action (..), Path (..), Selected (..))
import WTP.Value
import Prelude hiding (Bool (..), not)

newtype ObservedState = ObservedState (HashMap Query [Value])
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
prettyTrace (Trace []) = "(empty trace)"
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
      TraceState effect state ->
        annotate (effect `stutterColorOr` Blue <> bold) (pretty i <> "." <+> "State")
          <> line
          <> indent 2 (prettyObservedState state)
    Stutter `stutterColorOr` _ = colorDull Black
    NoStutter `stutterColorOr` fallback = color fallback

prettyObservedState :: ObservedState -> Doc AnsiStyle
prettyObservedState (ObservedState state)
  | HashMap.null state = "(empty state)"
  | otherwise =
    vsep
      ( withValues
          ( \(query, values) ->
              bullet <+> align (prettyQuery query <> line <> indent 2 (vsep (map (("-" <+>) . prettyValue) values)))
          )
      )
  where
    withValues :: ((Query, [Value]) -> s) -> [s]
    withValues f =
      state
        & HashMap.toList
        & List.sortBy (comparing fst)
        & map f

prettyQuery :: Query -> Doc AnsiStyle
prettyQuery = \case
  ByCss (Selector selector) -> "byCss" <+> pretty (show selector)
  Get state' sub -> prettyState state' <+> parens (prettyQuery sub)

prettyValue :: Value -> Doc AnsiStyle
prettyValue = \case
  VNull -> "null"
  VBool b -> pretty (show b)
  VElement el -> pretty (ref el)
  VString t -> pretty (show t)
  VNumber n -> pretty (show n)
  VSeq vs -> brackets (hsep (map prettyValue (Vector.toList vs)))
  VSet vs -> braces (hsep (map prettyValue (HashSet.toList vs)))
  VFunction (BuiltInFunction bif) -> prettyBuiltInFunction bif
  where
    prettyBuiltInFunction = \case
      FAnd -> "and"
      FOr -> "or"
      FNot -> "not"
      FIdentity -> "identity"
      FIn -> "in"
      FLength -> "length"
      FFilter -> "filter"
      FMap -> "map"
      FHead -> "head"
      FTail -> "tail"
      FInit -> "init"
      FLast -> "last"
      FParseNumber -> "parseNumber"
      FSplitOn -> "splitOn"
      FStrip -> "strip"

prettyState :: ElementState -> Doc AnsiStyle
prettyState = \case
  Attribute n -> "attribute" <+> pretty (show n)
  Property n -> "property" <+> pretty (show n)
  CssValue n -> "cssValue" <+> pretty (show n)
  Text -> "text"
  Enabled -> "enabled"
