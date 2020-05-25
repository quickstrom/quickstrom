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
    apply,
    identity,
    in',
    not,
    length,
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
import Data.Scientific (Scientific)
import Data.Text (Text)
import WTP.Element
import WTP.Formula
import WTP.Query
import WTP.Value
import Prelude ((.))

-- * Constructing Terms

num :: Scientific -> Formula
num = Literal . VNumber

lit :: Value -> Formula
lit = Literal

null :: Formula
null = lit VNull

string :: Text -> Formula
string = Literal . VString

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

apply :: Formula -> [Formula] -> Formula
apply = Apply

builtIn :: BuiltInFunction -> Formula
builtIn = Literal . VFunction . BuiltInFunction

not :: Formula
not = Literal (VFunction (BuiltInFunction FNot))

identity :: Formula
identity = builtIn FIdentity

in' :: Formula
in' = builtIn FIn

length :: Formula
length = builtIn FLength

filter :: Formula
filter = builtIn FFilter

map :: Formula
map = builtIn FMap

head :: Formula
head = builtIn FHead

tail :: Formula
tail = builtIn FTail

init :: Formula
init = builtIn FInit

last :: Formula
last = builtIn FLast

parseNumber :: Formula
parseNumber = builtIn FParseNumber

splitOn :: Formula
splitOn = builtIn FSplitOn

strip :: Formula
strip = builtIn FStrip
