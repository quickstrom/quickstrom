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

-- | 
module WTP.Query where

import WTP.Element
import Data.Typeable (Typeable)
import Control.Monad.Freer (Eff)

data QueryF t where
  QueryAll :: Selector -> QueryF [Element]
  Get :: (Eq a, Show a, Typeable a) => ElementState a -> Element -> QueryF a

newtype Query a = Query (Eff '[QueryF] a)
  deriving (Functor, Applicative, Monad)

instance Show (Query a) where
  show _ = "Query{}"
