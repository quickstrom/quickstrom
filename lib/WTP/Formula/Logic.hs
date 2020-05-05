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
import WTP.Value
import qualified WTP.Type as WTP
import Prelude hiding (False, True, not)
import Algebra.Lattice (Lattice(..), BoundedMeetSemiLattice(..), BoundedJoinSemiLattice(..))
import Algebra.Heyting (Heyting(..))
import Data.String (IsString(..))
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Data.Aeson as JSON

data Literal t where
  LTrue :: Literal 'WTP.Bool
  LFalse :: Literal 'WTP.Bool
  LString :: Text -> Literal 'WTP.String
  LJson :: JSON.Value -> Literal 'WTP.Json

deriving instance Eq (Literal t)
deriving instance Show (Literal t)

data Query t where
  QueryOne :: Selector -> Query 'WTP.Element
  QueryAll :: Selector -> Query ('WTP.Seq 'WTP.Element)
  Get :: ElementState a -> Query 'WTP.Element -> Query a
  Map :: (Query a -> Query b) -> Query ('WTP.Seq a) -> Query ('WTP.Seq b)

data Formula t where
  Literal :: Literal a -> Formula a
  Set :: [Formula a] -> Formula ('WTP.Set a)
  Seq :: [Formula a] -> Formula ('WTP.Seq a)
  Not :: Formula 'WTP.Bool -> Formula 'WTP.Bool
  And :: Formula 'WTP.Bool -> Formula 'WTP.Bool -> Formula 'WTP.Bool
  Or :: Formula 'WTP.Bool -> Formula 'WTP.Bool -> Formula 'WTP.Bool
  Always :: Formula 'WTP.Bool -> Formula 'WTP.Bool
  Query :: Typeable a => Query a -> Formula a
  Equals :: (a ~ b, Typeable a, Typeable b) => Formula a -> Formula b -> Formula 'WTP.Bool
  -- ForAll :: Formula (Set a) -> (FValue a -> Formula Bool) -> Formula Bool

  Apply :: (FValue a -> FValue b) -> Formula a -> Formula b

instance IsString (Formula 'WTP.String) where
  fromString = Literal . LString . Text.pack

type Proposition = Formula 'WTP.Bool

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

withQueries :: Monad m => (forall a. Typeable a => Query a -> m b) -> Formula c -> m [b]                                                                  
withQueries f = \case
  Literal{} -> pure []
  Set ps -> concat <$> traverse (withQueries f) ps
  Seq ps -> concat <$> traverse (withQueries f) ps
  Not p -> withQueries f p
  And p q -> (<>) <$> withQueries f p <*> withQueries f q
  Or p q -> (<>) <$> withQueries f p <*> withQueries f q
  Always p -> withQueries f p
  Equals p q -> (<>) <$> withQueries f p <*> withQueries f q
  Query query -> pure <$> f query
  Apply _ p -> withQueries f p
