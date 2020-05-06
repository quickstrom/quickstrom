{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module WTP.Formula.Logic where

import Data.Text (Text)
import WTP.Element
import Prelude hiding (False, True, not)
import Algebra.Lattice (Lattice(..), BoundedMeetSemiLattice(..), BoundedJoinSemiLattice(..))
import Algebra.Heyting (Heyting(..))
import Data.String (IsString(..))
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Data.Aeson as JSON
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Control.Monad.Freer (Eff)

data Literal t where
  LTrue :: Literal Bool
  LFalse :: Literal Bool
  LNum :: (Eq n, Show n, Num n) => n -> Literal n
  LString :: Text -> Literal Text
  LJson :: JSON.Value -> Literal JSON.Value

deriving instance Eq (Literal t)
deriving instance Show (Literal t)

type IsValue a = (Eq a, Show a, Typeable a)

data QueryF t where
  QueryAll :: Selector -> QueryF [Element]
  Get :: IsValue a => ElementState a -> Element -> QueryF a

newtype Query a = Query (Eff '[QueryF] a)
  deriving (Functor, Applicative, Monad)

type Set = HashSet

data Formula t where
  Literal :: Literal a -> Formula a
  Set :: (IsValue a, Hashable a) => [Formula a] -> Formula (Set a)
  Seq :: [Formula a] -> Formula [a]
  Not :: Formula Bool -> Formula Bool
  And :: Formula Bool -> Formula Bool -> Formula Bool
  Or :: Formula Bool -> Formula Bool -> Formula Bool
  Always :: Formula Bool -> Formula Bool
  BindQuery :: IsValue a => Query a -> Formula a
  Equals :: (a ~ b, IsValue a, IsValue b) => Formula a -> Formula b -> Formula Bool
  -- ForAll :: Formula (Set a) -> (FValue a -> Formula Bool) -> Formula Bool

  MapFormula :: (a -> b) -> Formula a -> Formula b

instance Functor Formula where
  fmap = MapFormula

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

simplify :: Formula a -> Formula a
simplify = \case
  And p q ->
    case (simplify p, simplify q) of
      (_, Literal LFalse) -> Literal LFalse
      (Literal LFalse, _) -> Literal LFalse
      (p', Literal LTrue) -> p'
      (Literal LTrue, p') -> p'
      (p', q') -> And p' q'
  Or p q ->
    case (simplify p, simplify q) of
      (_, Literal LTrue) -> Literal LTrue
      (Literal LTrue, _) -> Literal LTrue
      (p', Literal LFalse) -> p'
      (Literal LFalse, p') -> p'
      (p', q') -> Or p' q'
  Not (Literal LFalse) -> Literal LTrue
  Not (Literal LTrue) -> Literal LFalse
  Not (Not p) -> simplify p
  p -> p

withQueries :: (Monad m, IsValue b) => (forall q. IsValue q => Query q -> m b) -> Formula a -> m [b]                                                                  
withQueries f = \case
  Literal{} -> pure []
  Set ps -> concat <$> traverse (withQueries f) ps
  Seq ps -> concat <$> traverse (withQueries f) ps
  Not p -> withQueries f p
  And p q -> (<>) <$> withQueries f p <*> withQueries f q
  Or p q -> (<>) <$> withQueries f p <*> withQueries f q
  Always p -> withQueries f p
  Equals p q -> (<>) <$> withQueries f p <*> withQueries f q
  BindQuery query -> pure <$> f query
  MapFormula _ p -> withQueries f p
