{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Bool as Bool
import Data.Function ((&))
import qualified Data.Text as Text
import Data.Text (Text)
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.Tasty as Tasty
import WTP.Formula.Syntax
import qualified WTP.Run as WTP
import WTP.Specification
import Control.Lens ((^?), ix)
import Data.Maybe (fromMaybe)

cwd :: FilePath
cwd = unsafePerformIO getCurrentDirectory

main :: IO ()
main =
  Tasty.defaultMain $
    WTP.testSpecifications
      [ ("button", buttonSpec),
        ("comment form", commentFormSpec)
      ]

-- Simple example: a button that can be clicked, which then shows a message
buttonSpec :: Specification Formula
buttonSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/button.html"),
      actions =
        [(1,  Click "button")],
      property =
        Always buttonIsEnabled
          \/ buttonIsEnabled `Until` (".message" `hasText` "Boom!" ∧ Not buttonIsEnabled)
    }

commentFormSpec :: Specification Formula
commentFormSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/comment-form.html"),
      actions =
        [ (2, Click "input[type=submit]"),
          (1, Focus "input[type=text]"),
          (5, KeyPress 'a')
        ]
          -- <> (KeyPress <$> ['\0' .. '\127'])
          ,
      property =
        let commentPosted = isVisible ".comment-display" ∧ commentIsValid ∧ Not (isVisible "form")
            invalidComment = Not (isVisible ".comment-display") /\ isVisible "form"
         in Always (isVisible "form")
              \/ isVisible "form" `Until` (commentPosted \/ invalidComment)
    }

buttonIsEnabled :: Formula
buttonIsEnabled = (traverse (get Enabled) =<< query "button") ≡ Just Bool.True

hasText :: Selector -> Text -> Formula
hasText sel message =
  (traverse (get Text) =<< query sel) ≡ Just message

isVisible :: Selector -> Formula
isVisible sel = Not ((traverse (get (CssValue "display")) =<< query sel) ≡ Just "none")

commentIsValid :: Formula
commentIsValid = (traverse (get Text) =<< query ".comment") ⊢ \case
  Just t ->
    t
      & Text.splitOn ": "
      & (^? (ix 1))
      & fromMaybe ""
      & Text.strip
      & (>= 3) . Text.length
  Nothing -> Bool.False
