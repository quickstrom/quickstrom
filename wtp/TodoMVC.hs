{-# LANGUAGE OverloadedStrings #-}

module TodoMVC
  ( spec,
  )
where

import Control.Lens (lastOf, lengthOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Helpers
import Text.Read (readMaybe)
import WTP.Specification
import WTP.Syntax
import Prelude hiding ((<), (<=), (>), (>=), all, init)

spec :: Specification Proposition
spec =
  Specification
    { origin = Path ("http://todomvc.com/examples/angularjs/"),
      actions =
        [ (5, [Focus ".todoapp .new-todo", KeyPress 'a', KeyPress '\xe006']), -- full sequence of creating a new item
          (4, [Focus ".todoapp .new-todo"]),
          (4, [KeyPress 'a']),
          (4, [Click ".todoapp .filters a"]),
          (1, [Click ".todoapp .destroy"])
        ],
      proposition = init /\ (always (enterBlank \/ enterText \/ addNew \/ changeFilter))
    }
  where
    init = isEmpty
    enterBlank =
      pendingText === next pendingText
        /\ itemTexts === next itemTexts -- TODO: stutter invariance not working, this shouldn't be needed!
    enterText = neg (pendingText === next pendingText)
    changeFilter =
      neg (currentFilter === next currentFilter)
        /\ filterIs (Just All) ==> (numItems >= next numItems)
        /\ pendingText === next pendingText
    addNew =
      (Just <$> pendingText) === next lastItemText
        /\ next (pendingText === "")

data Filter = All | Active | Completed
  deriving (Eq, Read, Show)

-- * State helpers:

isEmpty :: Proposition
isEmpty = filterIs Nothing /\ (null <$> items)

currentFilter :: Formula (Maybe Filter)
currentFilter = query ((>>= (readMaybe . Text.unpack)) <$> (traverse text =<< one ".todoapp .filters .selected"))

filterIs :: Maybe Filter -> Proposition
filterIs f = (== f) <$> currentFilter

items :: Formula [Element]
items = query (all ".todo-list li")

itemTexts :: Formula [Text]
itemTexts = query (traverse text =<< all ".todo-list li label")

lastItemText :: Formula (Maybe Text)
lastItemText = lastOf traverse <$> itemTexts

numItems :: Formula Int
numItems = lengthOf traverse <$> items

pendingText :: Formula Text
pendingText = fromMaybe mempty <$> inputValue ".new-todo"
