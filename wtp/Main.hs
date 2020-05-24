{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Function ((&))
import qualified Data.Text as Text
import Helpers
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.QuickCheck as QuickCheck
import qualified Test.Tasty as Tasty
import qualified TodoMVC
import qualified WTP.Run as WTP
import WTP.Specification
import WTP.Syntax
import Prelude hiding ((<), (<=), (>), (>=), all, init, length, tail, head)

cwd :: FilePath
cwd = unsafePerformIO getCurrentDirectory

main :: IO ()
main =
  Tasty.defaultMain $
    WTP.testSpecifications
      [ ("button", buttonSpec),
        ("ajax", ajaxSpec),
        ("spinners", spinnersSpec),
        ("toggle", toggleSpec),
        ("comment form", commentFormSpec),
        ("TodoMVC AngularJS", TodoMVC.spec "angularjs"),
        ("TodoMVC Angular2", TodoMVC.spec "angular2"),
        ("TodoMVC React", TodoMVC.spec "react"),
        ("TodoMVC Vue.js", TodoMVC.spec "vue"),
        ("TodoMVC Backbone.js", TodoMVC.spec "backbone"),
        ("TodoMVC Mithril", TodoMVC.spec "mithril"),
        ("TodoMVC Shpadoinkle", (TodoMVC.spec mempty) {origin = Path "http://fresheyeball.gitlab.io/Shpadoinkle/"}),
        ("TodoMVC Moxie.rs", (TodoMVC.spec mempty) {origin = Path "https://moxie.rs/dom/examples/todo/index.html"})
      ]

-- Simple example: a button that can be clicked, which then shows a message
buttonSpec :: Specification Formula
buttonSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/button.html"),
      readyWhen = "button",
      actions = clicks,
      proposition =
        let click = buttonIsEnabled /\ next (".message" `hasText` "Boom!" /\ neg buttonIsEnabled)
         in buttonIsEnabled /\ always click
    }

ajaxSpec :: Specification Formula
ajaxSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/ajax.html"),
      readyWhen = "button",
      actions = clicks,
      proposition =
        let disabledLaunchWithMessage msg =
              ".message" `hasText` msg /\ neg buttonIsEnabled
            launch =
              buttonIsEnabled
                /\ next (disabledLaunchWithMessage "Missiles launched.")
            impactOrNoImpact =
              disabledLaunchWithMessage "Missiles launched."
                /\ next
                  ( disabledLaunchWithMessage "Boom"
                      \/ disabledLaunchWithMessage "Missiles did not hit target."
                  )
         in buttonIsEnabled /\ always (launch \/ impactOrNoImpact)
    }

spinnersSpec :: Specification Formula
spinnersSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/spinners.html"),
      readyWhen = "body",
      actions = clicks,
      proposition =
        let numberOfActiveSpinners = length (queryAll (byCss ".spinner.active"))
         in numberOfActiveSpinners === num 0 /\ always (numberOfActiveSpinners <= num 1)
    }

toggleSpec :: Specification Formula
toggleSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/toggle.html"),
      readyWhen = "button",
      actions = clicks,
      proposition =
        let on = "button" `hasText` "Turn me off"
            off = "button" `hasText` "Turn me on"
            turnOn = off /\ next on
            turnOff = on /\ next off
         in off /\ always (turnOn \/ turnOff)
    }

draftsSpec :: Specification Formula
draftsSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/drafts.html"),
      readyWhen = "button",
      actions = clicks,
      proposition = top
    }

commentFormSpec :: Specification Formula
commentFormSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/comment-form.html"),
      readyWhen = "form",
      actions = QuickCheck.oneof [clicks, asciiKeyPresses, foci],
      proposition =
        let commentPosted = isVisible ".comment-display" /\ commentIsValid /\ neg (isVisible "form")
            invalidComment = neg (isVisible ".comment-display") /\ isVisible "form"
            postComment = isVisible "form" /\ next (commentPosted \/ invalidComment)
         in isVisible "form" /\ always postComment
    }

buttonIsEnabled :: Formula
buttonIsEnabled = queryOne (enabled (byCss "button")) === top

commentIsValid :: Formula
commentIsValid = commentLength (queryOne (text (byCss ".comment"))) >= num 3
  where
    commentLength t =
        t
          & splitOn ": "
          & tail
          & head
          & strip
          & length
