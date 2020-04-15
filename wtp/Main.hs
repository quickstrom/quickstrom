{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Bool as Bool
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Hedgehog as Hedgehog
import qualified Hedgehog.Main as Hedgehog
import System.Directory
import WTP.Formula.Syntax
import qualified WTP.Run as WTP
import WTP.Specification

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  Hedgehog.defaultMain (Hedgehog.check <$> props cwd)

props :: FilePath -> [Hedgehog.Property]
props cwd =
  [ Hedgehog.withTests 1 (WTP.asProperty buttonSpec),
    Hedgehog.withTests 1 (WTP.asProperty commentFormSpec)
  ]
  where
    -- Simple example: a button that can be clicked, which then shows a message
    buttonSpec =
      Specification
        { origin = Path ("file://" <> Text.pack cwd <> "/test/button.html"),
          actions =
            [Click "button"],
          property =
            buttonIsEnabled
              `Until` (".message" `hasText` "Boom!" ∧ Not buttonIsEnabled)
        }
    commentFormSpec =
      Specification
        { origin = Path ("file://" <> Text.pack cwd <> "/test/comment-form.html"),
          actions = [Focus "input[type=text]", KeyPress ' ', Click "input[type=submit]"],
          property =
            let commentPosted = isVisible ".comment-display" ∧ Not commentIsBlank ∧ Not (isVisible "form")
                invalidComment = Not (isVisible ".comment-display") /\ isVisible "form" -- /\ ".error-message" `hasText` "Invalid comment!"
             in Not (isVisible ".comment-display") `Until` (commentPosted \/ invalidComment)
        }

buttonIsEnabled :: Formula
buttonIsEnabled = (traverse (get Enabled) =<< query "button") ≡ Just Bool.True

hasText :: Selector -> Text -> Formula
hasText sel message =
  (traverse (get Text) =<< query sel) ≡ Just message

isVisible :: Selector -> Formula
isVisible sel = Not ((traverse (get (CssValue "display")) =<< query sel) ≡ Just "none")

commentIsBlank :: Formula
commentIsBlank = (traverse (get Text) =<< query ".comment") ≡ Just ""
