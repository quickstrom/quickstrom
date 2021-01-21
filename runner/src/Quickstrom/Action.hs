{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Quickstrom.Action where

import Data.Aeson (ToJSON)
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
  | Await Selector
  | AwaitWithTimeoutSecs Int Selector
  | Navigate Text
  deriving (Eq, Show, Generic, ToJSON)

data ActionSequence = Single (Action Selector) | Sequence [(Action Selector)]

type PotentialActionSequence = [Action Selector]
type SelectedActionSequence  = [Action Selected]

actionSequenceToPotentialActionSeq :: ActionSequence -> PotentialActionSequence
actionSequenceToPotentialActionSeq = \case
  Single ba -> [ba]
  Sequence s -> s

actionSequencesToPotentialActionSeqs :: Vector (Int, ActionSequence) -> Vector (Int, PotentialActionSequence)
actionSequencesToPotentialActionSeqs v =
  map (\(f,s) -> (f, actionSequenceToPotentialActionSeq s)) v
