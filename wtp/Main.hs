{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (last, Bool(..), all)
import Control.Lens ((^?), ix)
import Data.Aeson as JSON
import qualified Data.Bool as Bool
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Read as Text
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
buttonSpec :: Specification Proposition
buttonSpec =
  Specification
    { origin = Path ("file://" <> Text.pack cwd <> "/test/button.html"),
      actions =
        [(1, [Click "button"])],
      proposition =
        always (buttonIsEnabled \/ neg buttonIsEnabled)
        -- always (buttonIsEnabled \/ (".message" `hasText` "Boom!" /\ neg buttonIsEnabled))
          -- buttonIsEnabled `until` (".message" `hasText` "Boom!" /\ neg buttonIsEnabled)
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
        in always (on \/ off) -- TODO: primed versions to define actions
        -- Used to be:
        -- always (on `until` off \/ off `until` on)
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
         in always (isVisible "form")
              \/ always (isVisible "form" \/ (commentPosted \/ invalidComment))
              -- isVisible "form" `until` (commentPosted \/ invalidComment)
    }

todoMvcSpec :: Specification Proposition
todoMvcSpec =
  Specification
    { origin = Path ("http://todomvc.com/examples/angularjs/"),
      actions =
        [ (2, [Focus ".todoapp .new-todo", KeyPress 'a', KeyPress '\xe006']),
          (1, [Click ".todoapp .filters a"]),
          (1, [Click ".todoapp button"])
        ],
      proposition = isEmpty \/ (always (isEmpty \/ filterActive \/ filterCompleted \/ filterAll))
    }
    where
      filterIs f = query ((== f) <$> currentFilter) /\ correctNumberItemsLeft
      isEmpty = filterIs Nothing
      filterActive = filterIs (Just Active)
      filterCompleted = filterIs (Just Completed)
      filterAll = filterIs (Just All)

data Filter = All | Active | Completed
  deriving (Eq, Read, Show)

currentFilter :: Query (Maybe Filter)
currentFilter = (>>= (readMaybe . Text.unpack)) <$> (traverse text =<< one ".todoapp .filters .selected")

numberChecked :: Query Int
numberChecked = do
  boxes <- todoCheckboxes
  length . filter (== JSON.Bool Bool.True) <$> traverse (property "checked") boxes

numberUnchecked :: Query Int
numberUnchecked = do
  boxes <- todoCheckboxes
  length . filter (/= JSON.Bool Bool.True) <$> traverse (property "checked") boxes

todoCheckboxes :: Query [Element]
todoCheckboxes = all ".todoapp .todo-list input[type=checkbox]"

numberItemsLeft :: Query (Maybe Int)
numberItemsLeft = do
  t <- traverse text =<< one ".todoapp .todo-count strong"
  pure (t >>= parse)
  where
    parse = either (const Nothing) (Just . fst) . Text.decimal

correctNumberItemsLeft :: Proposition
correctNumberItemsLeft =
  query (isCorrect <$> numberUnchecked <*> numberItemsLeft)
  where
    isCorrect n t
      | n > 0 = Just n == t
      | otherwise = t == Nothing

buttonIsEnabled :: Proposition
buttonIsEnabled = fromMaybe bottom <$> query (traverse enabled =<< one "button")

hasText :: Selector -> Text -> Proposition
hasText sel message =
  (== Just message) <$> query (traverse text =<< one sel)

isVisible :: Selector -> Proposition
isVisible sel = neg ((== Just "none") <$> query (traverse (cssValue "display") =<< one sel))

commentIsValid :: Proposition
commentIsValid = query (traverse text =<< one ".comment") <&> \case
  Just t ->
    t
      & Text.splitOn ": "
      & (^? (ix 1))
      & fromMaybe ""
      & Text.strip
      & (>= 3) . Text.length
  Nothing -> Bool.False
