{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Quickstrom.Action where


import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Quickstrom.Element
import Quickstrom.Prelude
import Data.Bifunctor.TH (deriveBifunctor)
import Control.Lens (Traversal')

data Selected = Selected Selector Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON, Hashable)

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
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, ToJSON, Hashable)

data ActionSequence restSel firstSel = ActionSequence (Action firstSel) [Action restSel]
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, ToJSON, Hashable)

actionSequenceActions :: Traversal' (ActionSequence a a) (Action a)
actionSequenceActions f  (ActionSequence a as) = ActionSequence <$> f a <*> traverse f as

$(deriveBifunctor ''ActionSequence)

actionSequenceToNonEmpty :: ActionSequence s s -> NonEmpty (Action s)
actionSequenceToNonEmpty (ActionSequence a as) = a :| as

type PotentialActionSequence = [Action Selector]

type SelectedActionSequence = [Action Selected]

data Weighted a = Weighted {weight :: Int, weighted :: a}
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
