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

actionSumToPotentialActionSeq :: ActionSequence -> PotentialActionSequence
actionSumToPotentialActionSeq = \case
  Single ba -> [ba]
  Sequence s -> s

actionSumsToPotentialActionSeqs :: Vector (Int, ActionSequence) -> Vector (Int, PotentialActionSequence)
actionSumsToPotentialActionSeqs v =
  map (\(f,s) -> (f, actionSumToPotentialActionSeq s)) v
