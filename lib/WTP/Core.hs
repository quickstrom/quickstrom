{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Core where

import Data.Char (Char)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude ((<>), ($), (.), Eq, Maybe, Show(..))
import Data.Functor (Functor(..))

newtype Selector = Selector Text
  deriving (Show, IsString, Generic)

newtype Path = Path Text
  deriving (Show, IsString, Generic)

data Action = Focus Selector | KeyPress Char | Click Selector | Navigate Path
  deriving (Show, Generic)

data Attribute a where
  InnerHTML :: Attribute Text
  InnerText :: Attribute Text
  ClassList :: Attribute [Text]

deriving instance Show (Attribute a)

data Assertion a where
  Equals :: Eq a => a -> Assertion a
  Contains :: Text -> Assertion Text

deriving instance Show a => Show (Assertion a)

data Element = Element

data Query a where
  Query :: Selector -> Query (Maybe Element)
  QueryAll :: Selector -> Query [Element]
  Require :: Query (Maybe a) -> Query a
  Get :: Attribute a -> Query Element -> Query a
  Map :: (a -> b) -> Query a -> Query b

instance Functor Query where
    fmap f (Map g q) = Map (fmap f g) q
    fmap f q = Map f q

query :: Selector -> Query (Maybe Element)
query = Query

queryAll :: Selector -> Query [Element]
queryAll = QueryAll

require :: Query (Maybe a) -> Query a
require = Require

get :: Attribute a -> Query Element -> Query a
get = Get

instance Show (Query a) where
    show = \case
        Query selector -> "Query " <> show selector
        QueryAll selector -> "QueryAll " <> show selector
        Require query -> "Require " <> show query
        Get attribute query -> "Get " <> show attribute <> " " <> show query
        Map f query -> "Map _ " <> show query

data Formula where
  True :: Formula
  Not :: Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Until :: Formula -> Formula -> Formula
  Assert :: Show a => Query a -> Assertion a -> Formula

deriving instance Show Formula

true, false :: Formula
true = True
false = Not True

not, eventually, always :: Formula -> Formula
not = Not
eventually = until true
always = not . eventually . not

or, and, until, implies, equivalent, release :: Formula -> Formula -> Formula
or = Or
and p q = not (not p `or` not q)
until = Until
implies p q = not p `or` q
equivalent p q = (p `implies` q) `or` (q `implies` p)
release p q = not (not p `until` not q)

assert :: Show a => Query a -> Assertion a -> Formula
assert = Assert

infix 4 \/, /\

(/\), (\/) :: Formula -> Formula -> Formula
(/\) = and
(\/) = or

infix 5 ===

(===) :: (Show a, Eq a) => Query a -> a -> Formula
query === expected = assert query (Equals expected)

data Specification
  = Specification
      { actions :: [Action],
        correctness :: Formula
      }
  deriving (Show, Generic)

--
-- EXAMPLE
--

data SpinnerState = Active | Hidden

example :: Specification
example =
  Specification
    { actions = [Focus ".my-app input", KeyPress 'x', Click ".my-app button[type=submit]"],
      correctness =
        ( hasMessage
            "Post a comment below."
            ["message", "info"]
            /\ spinnerIs Hidden
        )
          `until` spinnerIs Active
          `until` ( always
                      ( hasMessage
                          "Failed to post comment."
                          ["message", "error"]
                          /\ spinnerIs Hidden
                      )
                      \/ always
                        ( hasMessage
                            "Form posted!"
                            ["message", "error"]
                            /\ spinnerIs Hidden
                        )
                  )
    }
  where
    spinnerIs state =
      get
        ClassList
        (require (query ".my-app .spinner"))
        === case state of
          Active -> ["spinner", "active"]
          Hidden -> ["spinner"]
    hasMessage message classes =
      ( get
          ClassList
          (require (query ".my-app .message"))
          === classes
      )
        `and` ( get InnerText (require (query ".my-app .message"))
                  === message
              )
