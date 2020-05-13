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

module WTP.Syntax
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
    next,
    always,
    seq,
    set,
    attribute,
    property,
    cssValue,
    text,
    enabled,
    byCss,
    queryAll,
    queryOne,
    (===),
    (/==),
    (<),
    (<=),
    (>),
    (>=),
  )
where

import Algebra.Heyting
import Algebra.Lattice
import qualified Data.Aeson as JSON
import Data.Hashable (Hashable)
import Data.Maybe (Maybe, listToMaybe)
import Data.Text (Text)
import WTP.Element
import WTP.Formula
import WTP.Query
import Prelude ((.), Bool, Eq, Num, Ord, Show, fmap)

num :: (Eq n, Show n, Num n) => n -> Formula n
num = Literal . LNum

json :: JSON.Value -> Formula JSON.Value
json = Literal . LJson

seq :: IsValue a => [Formula a] -> Formula [a]
seq = Seq

set :: (IsValue a, Hashable a) => [Formula a] -> Formula (Set a)
set = Set

next :: Formula a -> Formula a
next = Next

always :: Proposition -> Proposition
always = Always

attribute :: Text -> Query Element -> Query Text
attribute t = Get (Attribute t)

property :: Text -> Query Element -> Query JSON.Value
property t = Get (Property t)

cssValue :: Text -> Query Element -> Query Text
cssValue t = Get (CssValue t)

text :: Query Element -> Query Text
text = Get Text

enabled :: Query Element -> Query Bool
enabled = Get Enabled

byCss :: Selector -> Query Element
byCss = ByCss

queryAll :: IsValue a => Query a -> Formula [a]
queryAll = BindQuery

queryOne :: IsValue a => Query a -> Formula (Maybe a)
queryOne = fmap listToMaybe . queryAll

infixl 7 ===, /==

(===) :: IsValue a => Formula a -> Formula a -> Proposition
(===) = Equals

(/==) :: IsValue a => Formula a -> Formula a -> Proposition
a /== b = neg (a === b)

(<), (<=), (>), (>=) :: Ord a => Formula a -> Formula a -> Proposition
(<) = Compare LessThan
(<=) = Compare LessThanEqual
(>) = Compare GreaterThan
(>=) = Compare GreaterThanEqual
