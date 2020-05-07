{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Helpers
import qualified TodoMVC
import Control.Lens ((^?), ix)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.Tasty as Tasty
import qualified WTP.Run as WTP
import WTP.Specification
import WTP.Syntax
import Prelude hiding ((<), (>), (<=), (>=), all, init)

cwd :: FilePath
cwd = unsafePerformIO getCurrentDirectory

main :: IO ()
main =
  Tasty.defaultMain $
    WTP.testSpecifications
      [ ("button", buttonSpec),
        ("toggle", toggleSpec),
        ("comment form", commentFormSpec),
        ("TodoMVC AngularJS", TodoMVC.spec)
      ]

-- Simple example: a button that can be clicked, which then shows a message
buttonSpec :: Specification Proposition
buttonSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/button.html"),
      actions =
        [(1, [Click "button"])],
      proposition =
        let click = buttonIsEnabled /\ next (".message" `hasText` "Boom!" /\ neg buttonIsEnabled)
         in buttonIsEnabled /\ always click
    }

toggleSpec :: Specification Proposition
toggleSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/toggle.html"),
      actions =
        [(1, [Click "button"])],
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
      actions =
        [(1, [Click "button"])],
      proposition = top
    }

commentFormSpec :: Specification Proposition
commentFormSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/comment-form.html"),
      actions =
        [ (1, [Click "input[type=submit]"]),
          (1, [Focus "input[type=text]"]),
          (1, [KeyPress 'a']),
          (2, [Focus "input[type=text]", KeyPress 'a'])
        ],
      -- <> (KeyPress <$> ['\0' .. '\127'])

      proposition =
        let commentPosted = isVisible ".comment-display" /\ commentIsValid /\ neg (isVisible "form")
            invalidComment = neg (isVisible ".comment-display") /\ isVisible "form"
            postComment = isVisible "form" /\ next (commentPosted \/ invalidComment)
         in isVisible "form" /\ always postComment
    }

buttonIsEnabled :: Proposition
buttonIsEnabled = fromMaybe bottom <$> query (traverse enabled =<< one "button")


commentIsValid :: Proposition
commentIsValid = (commentLength <$> query (traverse text =<< one ".comment")) >= num 3
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
