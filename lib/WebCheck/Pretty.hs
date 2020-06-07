{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module WebCheck.Pretty
  ( prettyAction,
    prettyActions,
    prettyTrace,
    prettyQuery,
    prettyValue,
    prettySelected,
  )
where

import Data.Function ((&))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.Ord (comparing)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Symbols.Unicode (bullet)
import qualified Data.Vector as Vector
import WebCheck.Element
import WebCheck.Path
import WebCheck.Query
import WebCheck.Trace
import WebCheck.Value
import Data.Text.Prettyprint.Doc

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
