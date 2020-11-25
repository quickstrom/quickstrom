{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Quickstrom.Pretty
  ( prettyAction,
    prettyActions,
    prettyTrace,
    prettyValue,
    prettySelected,
  )
where

import qualified Data.Aeson as JSON
import Data.Function ((&))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Ord (comparing)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Symbols.Unicode (bullet)
import qualified Data.Vector as Vector
import Quickstrom.Element
import Quickstrom.Prelude
import Quickstrom.Trace

prettyAction :: Action Selected -> Doc AnsiStyle
prettyAction = \case
  Click sel -> "click" <+> prettySelected sel
  Focus sel -> "focus" <+> prettySelected sel
  KeyPress key -> "key press" <+> pretty (show key :: Text)
  EnterText t -> "enter text" <+> pretty (show t :: Text)
  Navigate uri -> "navigate to" <+> pretty uri

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
      TraceState effect state' ->
        annotate (effect `stutterColorOr` Blue <> bold) (pretty i <> "." <+> "State")
          <> line
          <> indent 2 (prettyObservedState state')
    Stutter `stutterColorOr` _ = colorDull Black
    NoStutter `stutterColorOr` fallback = color fallback

prettyObservedState :: ObservedState -> Doc AnsiStyle
prettyObservedState (ObservedState _ (ObservedElementStates states))
  | HashMap.null states = "(empty state)"
  | otherwise =
    vsep
      ( states
          & HashMap.toList
          & List.sortBy (comparing fst)
          & map
            ( \(selector, matchedElement) ->
                bullet
                  <+> align
                    ( pretty selector
                        <> line
                        <> indent 2 (vsep (map prettyMatchedElement matchedElement))
                    )
            )
      )
  where
    prettyMatchedElement stateValues =
      "-"
        <> align
          ( line
              <> indent 2 (vsep (map prettyStateValue (HashMap.toList stateValues)))
          )
    prettyStateValue (state'', value) = "-" <+> prettyState state'' <+> "=" <+> prettyValue value

prettyValue :: JSON.Value -> Doc AnsiStyle
prettyValue = \case
  JSON.Null -> "null"
  JSON.Bool b -> pretty (show b :: Text)
  JSON.String t -> pretty (show t :: Text)
  JSON.Number n -> pretty (show n :: Text)
  JSON.Array vs -> brackets (hsep (map prettyValue (Vector.toList vs)))
  JSON.Object obj ->
    encloseSep
      lbrace
      rbrace
      (comma <> space)
      (map (\(k, v) -> pretty k <> ":" <+> prettyValue v) (HashMap.toList obj))

prettyState :: ElementState -> Doc AnsiStyle
prettyState = \case
  Attribute n -> "attribute" <+> pretty (show n :: Text)
  Property n -> "property" <+> pretty (show n :: Text)
  CssValue n -> "cssValue" <+> pretty (show n :: Text)
  Text -> "text"
