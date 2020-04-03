{-# LANGUAGE DataKinds #-}
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

infix 4 \/, /\

(/\), (\/) :: Formula Full -> Formula Full -> Formula Full
(/\) = And
(\/) = Or

infix 5 ===

(===) :: (Show a, Eq a) => Query a -> a -> Formula Full
query === expected = Assert query (Equals expected)

--
-- EXAMPLE
--

data SpinnerState = Active | Hidden

example :: Property
example =
  Property
    { actions = [Focus ".my-app input", KeyPress 'x', Click ".my-app button[type=submit]"],
      specification =
        ( hasMessage
            "Post a comment below."
            ["message", "info"]
            /\ spinnerIs Hidden
        )
          `Until` spinnerIs Active
          `Until` ( Always
                      ( hasMessage
                          "Failed to post comment."
                          ["message", "error"]
                          /\ spinnerIs Hidden
                      )
                      \/ Always
                        ( hasMessage
                            "Form posted!"
                            ["message", "error"]
                            /\ spinnerIs Hidden
                        )
                  )
    }
  where
    spinnerIs state =
      Get
        ClassList
        (Require (Query ".my-app .spinner"))
        === case state of
          Active -> ["spinner", "active"]
          Hidden -> ["spinner"]
    hasMessage message classes =
      ( Get
          ClassList
          (Require (Query ".my-app .message"))
          === classes
      )
        `And` ( Get InnerText (Require (Query ".my-app .message"))
                  === message
              )
