{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (last, Bool(..))
import Control.Lens ((^?), ix)
import Control.Monad.Freer (Eff)
import Data.Aeson as JSON
import qualified Data.Bool as Bool
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Read as Text
import qualified Debug.Trace as Debug
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.Tasty as Tasty
import WTP.Formula.Syntax
import qualified WTP.Run as WTP
import WTP.Specification
import Text.Read (readMaybe)

cwd :: FilePath
cwd = unsafePerformIO getCurrentDirectory

main :: IO ()
main =
  Tasty.defaultMain $
    WTP.testSpecifications
      [ ("button", buttonSpec),
        ("toggle", toggleSpec),
        ("comment form", commentFormSpec),
        ("TodoMVC AngularJS", todoMvcSpec)
      ]

-- Simple example: a button that can be clicked, which then shows a message
buttonSpec :: Specification Formula
buttonSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/toggle.html"),
      actions =
        [(1, [Click "button"])],
      property =
        Always buttonIsEnabled
          \/ buttonIsEnabled `Until` (".message" `hasText` "Boom!" ∧ Not buttonIsEnabled)
    }

toggleSpec :: Specification Formula
toggleSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/toggle.html"),
      actions =
        [(1, [Click "button"])],
      property =
        let on = "button" `hasText` "Turn me off"
            off = "button" `hasText` "Turn me on"
        in (on `foreverOr` Next off) \/ (off `foreverOr` Next on)
    }

foreverOr :: Formula -> Formula -> Formula
p `foreverOr` q = p /\ (Always p \/ q)

commentFormSpec :: Specification Formula
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

      property =
        let commentPosted = isVisible ".comment-display" ∧ commentIsValid ∧ Not (isVisible "form")
            invalidComment = Not (isVisible ".comment-display") /\ isVisible "form"
         in Always (isVisible "form")
              \/ isVisible "form" `Until` (commentPosted \/ invalidComment)
    }

todoMvcSpec :: Specification Formula
todoMvcSpec =
  Specification
    { origin = Path ("http://todomvc.com/examples/angularjs/"),
      actions =
        [ (2, [Focus ".todoapp .new-todo", KeyPress 'a', KeyPress '\xe006']),
          (1, [Click ".todoapp .filters a"]),
          (1, [Click ".todoapp button"])
        ],
      property = isEmpty `Until` (Always (isEmpty \/ filterActive \/ filterCompleted \/ filterAll))
    }
    where
      isEmpty = currentFilter === Nothing
      filterActive = currentFilter === Just Active /\ correctNumberItemsLeft
      filterCompleted = currentFilter === Just Completed
      filterAll = currentFilter === Just All

data Filter = All | Active | Completed
  deriving (Eq, Read, Show)

currentFilter :: Eff '[Query] (Maybe Filter)
currentFilter = (>>= (readMaybe . Text.unpack)) <$> (traverse (get Text) =<< query ".todoapp .filters .selected")

numberChecked :: Eff '[Query] Int
numberChecked = do
  boxes <- todoCheckboxes
  length . filter (== JSON.Bool Bool.True) <$> traverse (get (Property "checked")) boxes

numberUnchecked :: Eff '[Query] Int
numberUnchecked = do
  boxes <- todoCheckboxes
  length . filter (/= JSON.Bool Bool.True) <$> traverse (get (Property "checked")) boxes

todoCheckboxes :: Eff '[Query] [Element]
todoCheckboxes = queryAll ".todoapp .todo-list input[type=checkbox]"

numberItemsLeft :: Eff '[Query] (Maybe Int)
numberItemsLeft = do
  t <- traverse (get Text) =<< query ".todoapp .todo-count strong"
  pure (t >>= parse)
  where
    parse = either (const Nothing) (Just . fromIntegral . fst) . Text.decimal

correctNumberItemsLeft :: Formula
correctNumberItemsLeft =
  ((,) <$> numberUnchecked <*> numberItemsLeft) |- isCorrect
  where
    isCorrect (n, t)
      | n > 0 = Just n == t
      | otherwise = t == Nothing

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
