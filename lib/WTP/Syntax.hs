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
  ( Formula,
    Query,
    Element,
    Selector,
    Lattice (..),
    Heyting (..),
    PropertyValue (..),
    top,
    bottom,
    lit,
    null,
    num,
    string,
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
    identity,
    length,
    isEmpty,
    filter,
    map,
    head,
    tail,
    init,
    last,
    parseNumber,
    splitOn,
    strip,
  )
where

import Algebra.Heyting
import Algebra.Lattice
import qualified Data.Aeson as JSON
import Data.Scientific (Scientific)
import Data.Text (Text)
import WTP.Element
import WTP.Formula
import WTP.Query
import Prelude (undefined, (.))

-- * Constructing Terms

num :: Scientific -> Formula
num = Literal . JSON.Number

lit :: JSON.Value -> Formula
lit = Literal

null :: Formula
null = lit JSON.Null

string :: Text -> Formula
string = Literal . JSON.String

seq :: [Formula] -> Formula
seq = Seq

set :: [Formula] -> Formula
set = Set

-- * Temporal Operators

next :: Formula -> Formula
next = Next

always :: Formula -> Formula
always = Always

-- * Queries and Element States

attribute :: Text -> Query -> Query
attribute t = Get (Attribute t)

property :: Text -> Query -> Query
property t = Get (Property t)

cssValue :: Text -> Query -> Query
cssValue t = Get (CssValue t)

text :: Query -> Query
text = Get Text

enabled :: Query -> Query
enabled = Get Enabled

byCss :: Selector -> Query
byCss = ByCss

queryAll :: Query -> Formula
queryAll = BindQuery QueryAll

queryOne :: Query -> Formula
queryOne = BindQuery QueryOne

-- * Equality and Ordering

infixl 7 ===, /==

(===) :: Formula -> Formula -> Formula
(===) = Equals

(/==) :: Formula -> Formula -> Formula
a /== b = neg (a === b)

(<), (<=), (>), (>=) :: Formula -> Formula -> Formula
(<) = Compare LessThan
(<=) = Compare LessThanEqual
(>) = Compare GreaterThan
(>=) = Compare GreaterThanEqual

-- * Functions

identity :: Formula -> Formula
identity = undefined

length :: Formula -> Formula
length = undefined

isEmpty :: Formula -> Formula
isEmpty = (=== num 0) . length

filter :: (Formula -> Formula) -> Formula -> Formula
filter = undefined

map :: (Formula -> Formula) -> Formula -> Formula
map = undefined

head :: Formula -> Formula
head = undefined

tail :: Formula -> Formula
tail = undefined
  
init :: Formula -> Formula
init = undefined

last :: Formula -> Formula
last = undefined

parseNumber :: Formula -> Formula
parseNumber = undefined

splitOn :: Formula -> Formula -> Formula
splitOn = undefined

strip :: Formula -> Formula
strip = undefined
