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

data Actions = Single (BaseAction Selector) | Sequence [(BaseAction Selector)]

type PotentialActionSequence = [BaseAction Selector]
type SelectedActionSequence  = [BaseAction Selected]

actionsToPotentialActionSeq :: Actions -> PotentialActionSequence
actionsToPotentialActionSeq = \case
  Single ba -> [ba]
  Sequence s -> s

actionsToPotentialActionSeqs :: Vector (Int, Actions) -> Vector (Int, PotentialActionSequence)
actionsToPotentialActionSeqs v =
  map (\(f,s) -> (f, actionsToPotentialActionSeq s)) v
