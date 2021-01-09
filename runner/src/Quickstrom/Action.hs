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

data BaseAction sel
  = Focus sel
  | KeyPress Char
  | EnterText Text
  | Click sel
  | Await Selector
  | AwaitWithTimeoutSecs Int Selector
  | Navigate Text
  deriving (Eq, Show, Generic, ToJSON)

data ActionSum = Single (BaseAction Selector) | Sequence [(BaseAction Selector)]

type PotentialActionSequence = [BaseAction Selector]
type SelectedActionSequence  = [BaseAction Selected]

actionSumToPotentialActionSeq :: ActionSum -> PotentialActionSequence
actionSumToPotentialActionSeq = \case
  Single ba -> [ba]
  Sequence s -> s

actionSumsToPotentialActionSeqs :: Vector (Int, ActionSum) -> Vector (Int, PotentialActionSequence)
actionSumsToPotentialActionSeqs v =
  map (\(f,s) -> (f, actionSumToPotentialActionSeq s)) v
