{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Quickstrom.Action where

import Data.Aeson (ToJSON)
import Data.Vector (Vector)
import qualified Data.List.NonEmpty as NonEmpty
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
  | Await Selector
  | AwaitWithTimeoutSecs Int Selector
  | Navigate Text
  deriving (Eq, Show, Generic, ToJSON)

data ActionSequence sel = Single (Action sel) | Sequence (NonEmpty (Action sel))
  deriving (Eq, Show, Generic, ToJSON)

type PotentialActionSequence = [Action Selector]
type SelectedActionSequence = [Action Selected]

actionSequenceToList :: ActionSequence sel -> [Action sel]
actionSequenceToList = \case
  Single a -> [a]
  Sequence as -> NonEmpty.toList as

actionSequencesToLists :: Vector (Int, ActionSequence sel) -> Vector (Int, [Action sel])
actionSequencesToLists v =
  map (\(f, s) -> (f, actionSequenceToList s)) v
