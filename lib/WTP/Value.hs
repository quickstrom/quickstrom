{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
module WTP.Value where

import Algebra.Heyting (Heyting (..))
import Algebra.Lattice (BoundedMeetSemiLattice(..), BoundedJoinSemiLattice(..), Lattice (..))
import Data.Hashable (Hashable(..))
import WTP.Element
import qualified WTP.Type as WTP
import Data.Text (Text)
import Data.HashSet (HashSet)
import qualified Data.Aeson as JSON

data FValue (t :: WTP.Type) where
  VTrue :: FValue 'WTP.Bool
  VFalse :: FValue 'WTP.Bool
  VString :: Text -> FValue 'WTP.String
  VElement :: Element -> FValue 'WTP.Element
  VJson :: JSON.Value -> FValue 'WTP.Json
  VSet :: HashSet (FValue a) -> FValue ('WTP.Set a)
  VSeq :: [FValue a] -> FValue ('WTP.Seq a)

instance Hashable (FValue a) where
  hashWithSalt salt = \case
    VTrue -> salt `hashWithSalt` (1 :: Int)
    VFalse -> salt `hashWithSalt` (0 :: Int)
    VString t -> salt `hashWithSalt` t
    VElement el -> salt `hashWithSalt` el
    VJson j -> salt `hashWithSalt` j
    VSet s -> salt `hashWithSalt` s
    VSeq s -> salt `hashWithSalt` s

instance Lattice (FValue 'WTP.Bool) where
  VTrue /\ p = p
  p /\ VTrue = p
  VFalse /\ _ = VFalse

  VTrue \/ _ = VTrue
  _ \/ VTrue  = VTrue
  VFalse \/ p = p

instance BoundedJoinSemiLattice (FValue 'WTP.Bool) where
  bottom = VFalse

instance BoundedMeetSemiLattice (FValue 'WTP.Bool) where
  top = VTrue

instance Heyting (FValue 'WTP.Bool) where
  VFalse ==> _ = VTrue
  VTrue ==> q = q

deriving instance Show (FValue t)

deriving instance Eq (FValue t)

