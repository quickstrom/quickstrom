{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Syntax where

import Data.Char (Char)
import Data.Functor (Functor (..))
import Data.String (IsString)
import Data.Text (Text)
import WTP.Core
import Prelude (($), (.), (<>), Eq, Maybe, Show (..))

infix 4 \/, /\, ∧, ∨

(/\), (\/), (∧), (∨) :: Formula Full -> Formula Full -> Formula Full
(/\) = And
(\/) = Or
(∧) = And
(∨) = Or

infix 5 ===, ≡

(===), (≡) :: (Show a, Eq a) => Query a -> a -> Formula Full
query === expected = Assert query (Equals expected)
query ≡ expected = Assert query (Equals expected)

infix 6 ¬

(¬) :: Formula Full -> Formula Full
(¬) = Not

--
-- EXAMPLE
--

data SpinnerState = Active | Hidden

-- Simple example, a form for posting a comment. Note that you can only post once, even 
-- if there's an error.
example :: Property
example =
  Property
    { actions = [Focus ".my-app input", KeyPress 'x', Click ".my-app button[type=submit]"],
      specification =
        ( hasMessage
            "Post a comment below."
            ["message", "info"]
            ∧ spinnerIs Hidden
        )
          `Until` spinnerIs Active
          `Until` ( Always
                      ( hasMessage
                          "Failed to post comment."
                          ["message", "error"]
                          ∧ spinnerIs Hidden
                      )
                      ∨ Always
                        ( hasMessage
                            "Form posted!"
                            ["message", "error"]
                            ∧ spinnerIs Hidden
                        )
                  )
    }
  where
    spinnerIs state =
      Get
        ClassList
        (Require (Query ".my-app .spinner"))
        ≡ case state of
          Active -> ["spinner", "active"]
          Hidden -> ["spinner"]
    hasMessage message classes =
      ( Get
          ClassList
          (Require (Query ".my-app .message"))
          ≡ classes
      )
        ∧ ( Get InnerText (Require (Query ".my-app .message"))
              ≡ message
          )
