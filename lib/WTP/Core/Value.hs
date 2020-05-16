{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module WTP.Core.Value where

import Data.HashSet (HashSet)
import Data.Scientific (Scientific)
import Data.Text (Text)
import WTP.Element (Element)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

type VSet = HashSet

data Value
  = VText Text
  | VNumber Scientific
  | VBoolean Bool
  | VString Text
  | VElement Element
  | VSeq [Value]
  | VSet (VSet Value)
  deriving (Show, Eq, Generic, Hashable)

compareWith :: Value -> Value -> Maybe Ordering
compareWith v1 v2 = case (v1, v2) of
   (VText t1, VText t2) -> pure (t1 `compare` t2)
   (VNumber t1, VNumber t2) -> pure (t1 `compare` t2)
   (VBoolean t1, VBoolean t2) -> pure (t1 `compare` t2)
   (VString t1, VString t2) -> pure (t1 `compare` t2)
   (VElement t1, VElement t2) -> pure (t1 `compare` t2)
   (VSeq _, VSeq _) -> Nothing -- TODO: support this?
   (VSet _, VSet _) -> Nothing -- TODO: support this?
   _ -> Nothing
