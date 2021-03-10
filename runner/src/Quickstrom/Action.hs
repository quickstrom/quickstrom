{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Quickstrom.Action where

import Data.Aeson (ToJSON)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Quickstrom.Element
import Quickstrom.Prelude

data Selected = Selected Selector Int
  deriving (Eq, Show, Generic, ToJSON)

data Action sel
  = Focus sel
  | KeyPress Char
  | EnterText Text
  | Click sel
  | Clear sel
  | Await Selector
  | AwaitWithTimeoutSecs Int Selector
  | Navigate Text
  | Refresh
  -- `Back` and `Forward` can't be supported, as the history cannot be introspected to validate if these actions are possible.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, ToJSON)

newtype ActionSequence sel = ActionSequence (NonEmpty (Action sel))
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, ToJSON)

type PotentialActionSequence = [Action Selector]

type SelectedActionSequence = [Action Selected]

actionSequenceToList :: ActionSequence sel -> [Action sel]
actionSequenceToList (ActionSequence actions') = NonEmpty.toList actions'

actionSequencesToLists :: Vector (Int, ActionSequence sel) -> Vector (Int, [Action sel])
actionSequencesToLists = map (second actionSequenceToList)

data Weighted a = Weighted {weight :: Int, weighted :: a}
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
