{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Bool as Bool
import qualified Data.Text as Text
import Data.Text (Text)
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.Tasty as Tasty
import WTP.Formula.Syntax
import qualified WTP.Run as WTP
import WTP.Specification

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
        [Focus "input", Click "button"],
      property =
        Always buttonIsEnabled
          \/ buttonIsEnabled `Until` (".message" `hasText` "Boom!" ∧ Not buttonIsEnabled)
    }

commentFormSpec :: Specification Formula
commentFormSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/comment-form.html"),
      actions =
        [ Click "button",
          Click "input[type=submit]",
          KeyPress ' ',
          Focus "input[type=text]"
        ],
      property =
        let commentPosted = isVisible ".comment-display" ∧ Not commentIsBlank ∧ Not (isVisible "form")
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

commentIsBlank :: Formula
commentIsBlank = (traverse (get Text) =<< query ".comment") ⊢ \case
  Just c -> Text.null (Text.strip c)
  Nothing -> Bool.True
