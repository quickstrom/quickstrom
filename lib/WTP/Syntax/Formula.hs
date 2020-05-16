{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Syntax.Formula where

import Algebra.Heyting (Heyting (..))
import Algebra.Lattice (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))
import qualified Data.Aeson as JSON
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.String (IsString (..))
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Typeable (Typeable)
import WTP.Syntax.Query
import Prelude hiding (not)

data Literal t where
  LTrue :: Literal Bool
  LFalse :: Literal Bool
  LNum :: (Eq n, Show n, Num n) => n -> Literal n
  LString :: Text -> Literal Text
  LJson :: JSON.Value -> Literal JSON.Value

deriving instance Eq (Literal t)

deriving instance Show (Literal t)

type IsValue a = (Eq a, Show a, Typeable a)

type Set = HashSet

data Comparison
  = LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual

data Formula t where
  Literal :: Literal a -> Formula a
  Set :: (IsValue a, Hashable a) => [Formula a] -> Formula (Set a)
  Seq :: IsValue a => [Formula a] -> Formula [a]
  Not :: Formula Bool -> Formula Bool
  And :: Formula Bool -> Formula Bool -> Formula Bool
  Or :: Formula Bool -> Formula Bool -> Formula Bool
  Next :: Formula a -> Formula a
  Always :: Formula Bool -> Formula Bool
  QueryAll :: IsValue a => Query a -> Formula [a]
  QueryOne :: IsValue a => Query a -> Formula (Maybe a)
  Equals :: (a ~ b, IsValue a, IsValue b) => Formula a -> Formula b -> Formula Bool
  Compare :: Ord a => Comparison -> Formula a -> Formula a -> Formula Bool
  -- ForAll :: Formula (Set a) -> (FValue a -> Formula Bool) -> Formula Bool

  -- MapFormula :: (a -> b) -> Formula a -> Formula b

instance Show (Formula a) where
  show = \case
    Literal l -> show l
    Set ps -> show ps
    Seq ps -> show ps
    Not p -> "(Not " <> show p <> ")"
    And p q -> "(And " <> show p <> " " <> show q <> ")"
    Or p q -> "(Or " <> show p <> " " <> show q <> ")"
    Next p -> "(Next " <> show p <> ")"
    Always p -> "(Always " <> show p <> ")"
    QueryAll q -> "(QueryAll " <> show q <> ")"
    QueryOne q -> "(QueryOne " <> show q <> ")"
    Equals p q -> "(Equals " <> show p <> " " <> show q <> ")"
    Compare comp p q ->
      let op = case comp of
            LessThan -> "<"
            LessThanEqual -> "<="
            GreaterThan -> ">"
            GreaterThanEqual -> ">="
       in "(" <> show p <> " " <> op <> " " <> show q <> ")"

-- MapFormula _ p -> "(MapFormula _ " <> show p <> ")"

-- instance Functor Formula where
--   fmap = MapFormula

instance IsString (Formula Text) where
  fromString = Literal . LString . Text.pack

type Proposition = Formula Bool

instance Lattice Proposition where

  (/\) = And

  (\/) = Or

instance BoundedJoinSemiLattice Proposition where
  bottom = Literal LFalse

instance BoundedMeetSemiLattice Proposition where
  top = Literal LTrue

instance Heyting Proposition where

  p ==> q = Not p `Or` q

  neg = Not
