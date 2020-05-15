{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens ((^?), ix)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Helpers
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.Tasty as Tasty
import qualified TodoMVC
import qualified WTP.Run as WTP
import WTP.Specification
import WTP.Syntax
import Prelude hiding ((<), (<=), (>), (>=), all, init)

cwd :: FilePath
cwd = unsafePerformIO getCurrentDirectory

main :: IO ()
main =
  Tasty.defaultMain $
    WTP.testSpecifications
      [ ("button", buttonSpec),
        ("ajax", ajaxSpec),
        ("toggle", toggleSpec),
        ("comment form", commentFormSpec),
        ("TodoMVC AngularJS", TodoMVC.spec "angularjs"),
        ("TodoMVC React", TodoMVC.spec "react"),
        ("TodoMVC Vue.js", TodoMVC.spec "vue"),
        ("TodoMVC Backbone.js", TodoMVC.spec "backbone")
      ]

-- Simple example: a button that can be clicked, which then shows a message
buttonSpec :: Specification Proposition
buttonSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/button.html"),
      readyWhen = "button",
      actions =
        [(1, Click "button")],
      proposition =
        let click = buttonIsEnabled /\ next (".message" `hasText` "Boom!" /\ neg buttonIsEnabled)
         in buttonIsEnabled /\ always click
    }

ajaxSpec :: Specification Proposition
ajaxSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/ajax.html"),
      readyWhen = "button",
      actions =
        [(1, Click "button")],
      proposition =
        let launch =
              buttonIsEnabled
                /\ next (".message" `hasText` "Missiles launched." /\ neg buttonIsEnabled)
            impact =
              (".message" `hasText` "Missiles launched." /\ neg buttonIsEnabled)
                /\ next (".message" `hasText` "Boom!" /\ neg buttonIsEnabled)
         in buttonIsEnabled /\ always (launch \/ impact)
    }

toggleSpec :: Specification Proposition
toggleSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/toggle.html"),
      readyWhen = "button",
      actions =
        [(1, Click "button")],
      proposition =
        let on = "button" `hasText` "Turn me off"
            off = "button" `hasText` "Turn me on"
            turnOn = off /\ next on
            turnOff = on /\ next off
         in off /\ always (turnOn \/ turnOff)
    }

draftsSpec :: Specification Proposition
draftsSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/drafts.html"),
      readyWhen = "button",
      actions =
        [(1, Click "button")],
      proposition = top
    }

commentFormSpec :: Specification Proposition
commentFormSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/comment-form.html"),
      readyWhen = "form",
      actions =
        [ (1, Click "input[type=submit]"),
          (1, Focus "input[type=text]"),
          (1, KeyPress 'a'),
          (2, Focus "input[type=text]")
        ],
      -- <> (KeyPress <$> ['\0' .. '\127'])

      proposition =
        let commentPosted = isVisible ".comment-display" /\ commentIsValid /\ neg (isVisible "form")
            invalidComment = neg (isVisible ".comment-display") /\ isVisible "form"
            postComment = isVisible "form" /\ next (commentPosted \/ invalidComment)
         in isVisible "form" /\ always postComment
    }

buttonIsEnabled :: Proposition
buttonIsEnabled = fromMaybe bottom <$> queryOne (enabled (byCss "button"))

commentIsValid :: Proposition
commentIsValid = (commentLength <$> queryOne (text (byCss ".comment"))) >= num 3
  where
    commentLength = \case
      Just t ->
        t
          & Text.splitOn ": "
          & (^? (ix 1))
          & fromMaybe ""
          & Text.strip
          & Text.length
      Nothing -> 0
