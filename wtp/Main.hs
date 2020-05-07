{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens ((^?), ix, lastOf)
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
import Text.Read (readMaybe)
import qualified WTP.Run as WTP
import WTP.Specification
import WTP.Syntax
import Prelude hiding (Bool (..), all, init)

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

todoMvcSpec :: Specification Proposition
todoMvcSpec =
  Specification
    { origin = Path ("http://todomvc.com/examples/angularjs/"),
      actions =
        [ (2, [Focus ".todoapp .new-todo", KeyPress 'a', KeyPress '\xe006']),
          (1, [Click ".todoapp .filters a"]),
          (1, [Click ".todoapp button"])
        ],
      proposition =
        let filterIs f = query ((== f) <$> currentFilter)
            items = query (all ".todo-list li")
            isEmpty = filterIs Nothing /\ (null <$> items)
            lastItemText = lastOf traverse <$> query (traverse text =<< all ".todo-list li label")
            init = isEmpty
            pendingText = inputValue ".new-todo"
            enterBlank = pendingText === next pendingText -- TODO: stutter invariance!
            enterText = neg (pendingText === next pendingText)
            addNew =
              pendingText === next lastItemText
                /\ next (filterIs (Just All))
                /\ next ((== Just "") <$> pendingText)
         in init /\ (always (enterBlank \/ enterText \/ addNew))
    }
  where

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

inputValue :: Selector -> Formula (Maybe Text)
inputValue sel =
  query (traverse (property "value") =<< one sel)
    <&> fmap fromJSON
    <&> ( >>=
            \case
              JSON.Success a -> Just a
              JSON.Error {} -> Nothing
        )

isVisible :: Selector -> Proposition
isVisible sel = (== Just "block") <$> query (traverse (cssValue "display") =<< one sel)

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
