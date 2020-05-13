{-# LANGUAGE OverloadedStrings #-}

module TodoMVC
  ( spec,
  )
where

import Control.Lens (lastOf, lengthOf)
import Data.Aeson as JSON
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Read as Text
import Helpers
import Text.Read (readMaybe)
import WTP.Specification
import WTP.Syntax
import Prelude hiding ((<), (<=), (>), (>=), all, init)

spec :: Text -> Specification Proposition
spec name =
  Specification
    { origin = Path ("http://todomvc.com/examples/" <> name <> "/"),
      readyWhen = ".todoapp",
      actions =
        [ (2, KeyPress 'a'),
          (2, KeyPress '\xe006'),
          (2, Focus ".todoapp .new-todo"),
          (2, Click ".todoapp .filters a"),
          (1, Click ".todoapp label[for=toggle-byCss]"),
          (1, Click ".todoapp .destroy")
        ],
      proposition = init /\ (always (enterText \/ addNew \/ changeFilter \/ toggleAll))
    }
  where
    init = isEmpty
    enterText =
      pendingText /== next pendingText
        /\ itemTexts === next itemTexts
    changeFilter =
      currentFilter /== next currentFilter
        /\ filterIs (Just All) ==> (numItems >= next numItems)
        /\ ( next
               ( filterIs (Just Active)
                   ==> ( numItemsLeft === numUnchecked
                           /\ numItems === numUnchecked
                       )
               )
           )
        -- NOTE: AngularJS and Mithril implementations are
        -- inconsistent with the other JS implementations, in that
        -- they clear the input field when the filter is changed.
        /\ if name `elem` ["angularjs", "mithril"] then top else pendingText === next pendingText
    addNew =
      pendingText === next lastItemText
        /\ next ((== Nothing) <$> pendingText)
    toggleAll =
      ( filterIs (Just All)
          ==> numItems === next numItems /\ next (numItems === numChecked)
      )
        \/ ( filterIs (Just Active)
               ==> ( numItems > num 0 ==> next numItems === num 0
                       \/ (numItems === num 0) ==> next numItems > num 0
                   )
           )

data Filter = All | Active | Completed
  deriving (Eq, Read, Show)

-- * State helpers:

isEmpty :: Proposition
isEmpty = filterIs Nothing /\ (null <$> items) /\ ((== Just "") <$> pendingText)

currentFilter :: Formula (Maybe Filter)
currentFilter = (>>= (readMaybe . Text.unpack)) <$> queryOne (text (byCss ".todoapp .filters .selected"))

filterIs :: Maybe Filter -> Proposition
filterIs f = (== f) <$> currentFilter

items :: Formula [Element]
items = queryAll (byCss ".todo-list li")

itemTexts :: Formula [Text]
itemTexts = queryAll (text (byCss ".todo-list li label"))

lastItemText :: Formula (Maybe Text)
lastItemText = lastOf traverse <$> itemTexts

numItems :: Formula Int
numItems = lengthOf traverse <$> items

checked :: Formula [Bool]
checked = (map (== JSON.Bool top)) <$> queryAll (property "checked" (byCss ".todo-list li label"))

numUnchecked :: Formula Int
numUnchecked = length . filter neg <$> checked

numChecked :: Formula Int
numChecked = length . filter id <$> checked

pendingText :: Formula (Maybe Text)
pendingText = inputValue ".new-todo"

numItemsLeft :: Formula Int
numItemsLeft =
  queryOne (text (byCss ".todoapp .todo-count strong"))
    <&> \t -> fromMaybe 0 (t >>= parse)
  where
    parse = either (const Nothing) (Just . fst) . Text.decimal
