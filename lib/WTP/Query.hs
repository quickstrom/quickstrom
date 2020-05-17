{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module WTP.Query where

import Data.Aeson as JSON
import Data.Text (Text)
import Data.Typeable (Typeable)
import WTP.Element

data Query t where
  ByCss :: Selector -> Query Element
  Get :: (Eq a, Show a, Typeable a) => ElementState a -> Query Element -> Query a

deriving instance Show (Query a)

instance JSON.ToJSON (Query a) where
  toJSON = \case
    ByCss (Selector selector) -> object ["tag" .= ("element" :: Text), "selector" .= selector]
    Get state sub -> object ["elementState" .= toJSON sub, "stateQuery" .= toJSON state]
