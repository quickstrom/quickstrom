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
    Proposition,
    Lattice (..),
    Heyting (..),
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
    map,
    query,
    (===),
  )
where

import Data.Text (Text)
import Prelude hiding (seq, all, map)
import Algebra.Heyting
import Algebra.Lattice
import WTP.Element
import WTP.Formula.Logic
import qualified WTP.Type as WTP
import Data.Typeable (Typeable)
import qualified Data.Aeson as JSON

json :: JSON.Value -> Formula 'WTP.Json
json = Literal . LJson

seq :: [Formula a] -> Formula ('WTP.Seq a)
seq = Seq

set :: [Formula a] -> Formula ('WTP.Set a)
set = Set

always :: Formula 'WTP.Bool -> Formula 'WTP.Bool
always = Always

attribute :: Text -> Query 'WTP.Element -> Query 'WTP.String
attribute t = Get (Attribute t)

property :: Text -> Query 'WTP.Element -> Query 'WTP.Json
property t = Get (Property t)

cssValue :: Text -> Query 'WTP.Element -> Query 'WTP.String
cssValue t = Get (CssValue t)

text :: Query 'WTP.Element -> Query 'WTP.String
text = Get Text

enabled :: Query 'WTP.Element -> Query 'WTP.Bool
enabled = Get Enabled

all :: Selector -> Query ('WTP.Seq 'WTP.Element)
all = QueryAll

one :: Selector -> Query 'WTP.Element
one = QueryOne

map :: (Query a -> Query b) -> Query ('WTP.Seq a)-> Query ('WTP.Seq b)
map = Map

query :: Typeable a => Query a -> Formula a
query = Query

(===) :: Typeable a => Formula a -> Formula a -> Proposition
(===) = Equals
