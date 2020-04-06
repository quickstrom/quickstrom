{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module WTP.Query where

import Control.Monad.Freer
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Typeable (Typeable)
import qualified Data.Aeson as JSON
import Data.Text (Text)

data ElementState a where
  Attribute :: Text -> ElementState Text
  Property :: Text -> ElementState JSON.Value
  CssValue :: Text -> ElementState Text
  Text :: ElementState Text
  Enabled :: ElementState Bool

deriving instance Eq (ElementState a)

deriving instance Show (ElementState a)


newtype Element = Element {ref :: Text}
  deriving (Eq, Show, Hashable)

newtype Selector = Selector Text
  deriving (Eq, Show, IsString, Generic, Hashable)

data Query a where
  Query :: Selector -> Query (Maybe Element)
  QueryAll :: Selector -> Query [Element]
  Get :: Typeable a => ElementState a -> Element -> Query a

query :: Member Query effs => Selector -> Eff effs (Maybe Element)
query = send . Query

queryAll :: Member Query effs => Selector -> Eff effs [Element]
queryAll = send . QueryAll

get :: (Member Query effs, Typeable a) => ElementState a -> Element -> Eff effs a
get attr = send . Get attr

