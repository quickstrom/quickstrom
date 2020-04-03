{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Syntax where

import Data.Char (Char)
import Data.String (IsString)
import Data.Text (Text)
import Prelude ((<>), ($), (.), Eq, Maybe, Show(..))
import Data.Functor (Functor(..))
import WTP.Core

query :: Selector -> Query (Maybe Element)
query = Query

queryAll :: Selector -> Query [Element]
queryAll = QueryAll

require :: Query (Maybe a) -> Query a
require = Require

get :: Attribute a -> Query Element -> Query a
get = Get

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
