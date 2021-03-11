{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Quickstrom.Pretty
  ( prettyAction,
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
import Data.Text.Prettyprint.Doc hiding (width)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Symbols.Unicode (bullet)
import qualified Data.Vector as Vector
import Quickstrom.Action
import Quickstrom.Element
import Quickstrom.Prelude
import Quickstrom.Trace

prettyAction :: Action Selected -> Doc AnsiStyle
prettyAction = \case
  Click sel -> "click" <+> prettySelected sel
  Clear sel -> "clear" <+> prettySelected sel
  Focus sel -> "focus" <+> prettySelected sel
  Await sel -> "await" <+> pretty (show sel :: Text)
  AwaitWithTimeoutSecs i sel -> "await secs" <+> pretty (show i :: Text) <+> pretty (show sel :: Text)
  KeyPress key -> "key press" <+> pretty (show key :: Text)
  EnterText t -> "enter text" <+> pretty (show t :: Text)
  Navigate uri -> "navigate to" <+> pretty uri
  Refresh -> "refresh"

prettyActionSeq :: ActionSequence Selected -> Doc AnsiStyle
prettyActionSeq (ActionSequence (action' :| [])) = prettyAction action'
prettyActionSeq (ActionSequence (action :| actions')) = "Sequence:" <> line <> indent 2 (vsep (zipWith item [1 ..] (action : toList actions')))
  where
    item :: Int -> Action Selected -> Doc AnsiStyle
    item i = \case
      ba -> (pretty i <> "." <+> prettyAction ba)

prettySelected :: Selected -> Doc AnsiStyle
prettySelected (Selected (Selector sel) i) = pretty sel <> brackets (pretty i)

prettyTrace :: Trace TraceElementEffect -> Doc AnsiStyle
prettyTrace (Trace []) = "(empty trace)"
prettyTrace (Trace elements') = vsep (zipWith prettyElement [1 ..] elements')
  where
    prettyElement :: Int -> TraceElement TraceElementEffect -> Doc AnsiStyle
    prettyElement i = \case
      TraceAction effect action result ->
        let annotation = case result of
              ActionSuccess -> effect `stutterColorOr` Magenta <> bold
              ActionFailed {} -> effect `stutterColorOr` Red <> bold
              ActionImpossible -> color Red <> bold
         in annotate annotation (pretty i <> "." <+> prettyActionSeq action)
      TraceState effect state' ->
        annotate (effect `stutterColorOr` Blue <> bold) (withSuffix effect (pretty i <> "." <+> "State"))
          <> line
          <> indent 2 (prettyObservedState state')
    Stutter `stutterColorOr` _ = color White
    NoStutter `stutterColorOr` fallback = color fallback

    withSuffix Stutter = (<+> "(stutter)")
    withSuffix NoStutter = identity

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
    prettyMatchedElement (ObservedElementState element' pos stateValues) =
      "-"
        <+> ( pretty element' <+> prettyPosition pos
                <> line
                <> indent 2 (vsep (map prettyStateValue (HashMap.toList stateValues)))
            )
    prettyStateValue (state'', value) = "-" <+> prettyState state'' <+> "=" <+> prettyValue value

prettyPosition :: Maybe Position -> Doc AnsiStyle
prettyPosition Nothing = "(no position)"
prettyPosition (Just pos) = parens (pretty (width pos) <> "x" <> pretty (height pos) <+> "at" <+> parens (pretty (x pos) <> ","  <+> pretty (y pos)))

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
