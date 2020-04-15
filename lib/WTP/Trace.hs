{-# LANGUAGE DataKinds #-}
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
    assertQuery,
    Trace (..),
    TraceElement (..),
    observedStates,
    prettyTrace,
  )
where

import Control.Lens
import Control.Monad.Freer
import qualified Data.Aeson.Text as JSON
import qualified Data.Bool as Bool
import Data.Function ((&))
import Data.Generics.Product (position)
import Data.Generics.Sum (_Ctor)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Symbols.Unicode (bullet)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Type.Reflection
import WTP.Assertion
import WTP.Query
import WTP.Result
import WTP.Specification (Action (..), Path (..))
import Prelude hiding (Bool (..), not)

data ElementStateValue where
  ElementStateValue :: (Typeable a, Show a, Eq a) => ElementState a -> a -> ElementStateValue

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
  deriving (Show)

runQueryInState :: ObservedState -> Eff '[Query] a -> a
runQueryInState ObservedState {queriedElements, elementStates} =
  run
    . interpret
      ( \case
          Query selector -> case HashMap.lookup selector queriedElements of
            Just (a : _) -> pure (Just a)
            _ -> pure Nothing
          QueryAll selector -> pure (fromMaybe [] (HashMap.lookup selector queriedElements))
          Get state element' ->
            let states = fromMaybe mempty (HashMap.lookup element' elementStates)
             in pure (fromJust (findElementState state states))
      )

assertQuery :: QueryAssertion -> ObservedState -> Result
assertQuery = \(QueryAssertion query' assertion) state ->
  let result' = runQueryInState state query'
   in runAssertion assertion result'

newtype Trace = Trace [TraceElement]
  deriving (Generic)


prettyAction :: Action -> Doc AnsiStyle
prettyAction = \case
  Click (Selector sel) -> "click" <+> pretty sel
  Focus (Selector sel) -> "focus" <+> pretty sel
  KeyPress key -> "key press" <+> pretty key
  Navigate (Path path) -> "navigate to" <+> pretty path

prettyTrace :: Trace -> Doc AnsiStyle
prettyTrace (Trace elements') = vsep (zipWith prettyElement [1..] elements')
    where
      prettyElement :: Int -> TraceElement -> Doc AnsiStyle
      prettyElement i = \case
        TraceAction action -> annotate (color Yellow <> bold) (pretty i <> "." <+> prettyAction action)
        TraceState state -> annotate (color Blue <> bold) (pretty i <> "." <+> "State") <> line <> indent 2 (prettyObservedState state)

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
    prettyElementStateValue :: ElementStateValue -> Doc ann
    prettyElementStateValue = \case
      (ElementStateValue (Attribute name) (Left b)) -> "attribute" <+> pretty name <> "=" <> if b then "true" else "false"
      (ElementStateValue (Attribute name) (Right t)) -> "attribute" <+> pretty name <> "=" <> dquotes (pretty t)
      (ElementStateValue (Property name) value) -> "property" <+> pretty name <+> "=" <+> pretty (JSON.encodeToLazyText value)
      (ElementStateValue (CssValue name) value) -> "css" <+> braces (pretty name <> ":" <+> pretty value <> ";")
      (ElementStateValue Text t) -> "text" <> "=" <> dquotes (pretty t)
      (ElementStateValue Enabled b) -> if b then "enabled" else "disabled"


          
data TraceElement
  = TraceAction Action
  | TraceState ObservedState
  -- TODO: `TraceEvent` when queried DOM nodes change
  deriving (Generic)



observedStates :: Monoid r => Getting r Trace ObservedState
observedStates = position @1 . traverse . _Ctor @"TraceState"
