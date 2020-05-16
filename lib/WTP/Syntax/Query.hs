{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module WTP.Syntax.Query where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import Data.Typeable (Typeable)
import WTP.Element

data ElementState a where
  Attribute :: Text -> ElementState Text
  Property :: Text -> ElementState JSON.Value
  CssValue :: Text -> ElementState Text
  Text :: ElementState Text
  Enabled :: ElementState Bool
  
deriving instance Show (ElementState a)

data Query t where
  ByCss :: Selector -> Query Element
  Get :: (Eq a, Show a, Typeable a) => ElementState a -> Query Element -> Query a

deriving instance Show (Query a)
