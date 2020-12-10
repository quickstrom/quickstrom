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
  | Navigate Text
  deriving (Eq, Show, Generic, ToJSON)

data ActionSum = Single (BaseAction Selector) | Sequence [(BaseAction Selector)]

type PotentialAction = [BaseAction Selector]
type Action = [BaseAction Selected]

actionSumToPotentialAction :: ActionSum -> PotentialAction
actionSumToPotentialAction = \case
  Single ba -> [ba]
  Sequence s -> s

actionSumsToPotentialActions :: Vector (Int, ActionSum) -> Vector (Int, PotentialAction)
actionSumsToPotentialActions v =
  map (\(f,s) -> (f, actionSumToPotentialAction s)) v
