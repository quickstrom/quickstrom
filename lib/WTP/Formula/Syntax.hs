{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Formula.Syntax
  ( Literal,
    Formula,
    Query,
    Proposition,
    Element,
    Selector,
    Lattice (..),
    Heyting (..),
    top,
    bottom,
    num,
    json,
    always,
    seq,
    set,
    attribute,
    property,
    cssValue,
    text,
    enabled,
    all,
    one,
    query,
    (===),
  )
where

import Algebra.Heyting
import Algebra.Lattice
import qualified Data.Aeson as JSON
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import WTP.Element
import WTP.Formula.Logic
import Prelude hiding (all, map, seq)
import Data.Hashable (Hashable)
import Control.Monad.Freer (send)

num :: (Eq n, Show n, Num n) => n -> Formula n
num = Literal . LNum

json :: JSON.Value -> Formula JSON.Value
json = Literal . LJson

seq :: IsValue a => [Formula a] -> Formula [a]
seq = Seq

set :: (IsValue a, Hashable a) => [Formula a] -> Formula (Set a)
set = Set

always :: Proposition -> Proposition
always = Always

attribute :: Text -> Element -> Query Text
attribute t = Query . send . Get (Attribute t)

property :: Text -> Element -> Query JSON.Value
property t = Query . send . Get (Property t)

cssValue :: Text -> Element -> Query Text
cssValue t = Query . send . Get (CssValue t)

text :: Element -> Query Text
text = Query . send . Get Text

enabled :: Element -> Query Bool
enabled = Query . send . Get Enabled

all :: Selector -> Query [Element]
all = Query . send . QueryAll

one :: Selector -> Query (Maybe Element)
one = fmap listToMaybe . all

query :: IsValue a => Query a -> Formula a
query = BindQuery

(===) :: IsValue a => Formula a -> Formula a -> Proposition
(===) = Equals
