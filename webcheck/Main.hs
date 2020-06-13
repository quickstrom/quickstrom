{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import qualified WebCheck.PureScript as WebCheck
import qualified WebCheck.PureScript.Program as WebCheck
import qualified WebCheck.Run as WebCheck
import qualified WebCheck.Specification as WebCheck

cwd :: FilePath
cwd = unsafePerformIO getCurrentDirectory

main :: IO ()
main =
  getArgs >>= \case
    [libraryPath, specPath] -> do
      specResult <- runExceptT $ do
        modules <- ExceptT (WebCheck.loadLibraryModules libraryPath)
        ExceptT (WebCheck.loadSpecificationFile modules specPath)
      case specResult of
        Left err -> do
          hPutStrLn stderr err
          exitWith (ExitFailure 1)
        Right spec ->
          putStrLn @Text ("We have a spec with queries: " <> show (WebCheck.specificationQueries spec))
          -- WebCheck.check spec
    [arg] | arg `elem` ["help", "--help", "-h"] -> usage
    _ -> do
      usage
      exitWith (ExitFailure 1)
  where
    usage :: IO ()
    usage = hPutStrLn stderr ("Usage: webcheck <LIBRARY_PATH> <SPEFICATION_PATH>" :: Text)
{-

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
        let numberOfActiveSpinners = apply length [queryAll (byCss ".spinner.active")]
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
    commentLength t = apply length [apply strip [apply head [apply tail [apply splitOn [": ", t]]]]]

-}
