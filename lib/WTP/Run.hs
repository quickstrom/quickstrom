{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module WTP.Run
  ( run,
  )
where

import qualified Control.Monad.Freer as Eff
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Natural (type (~>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Text (Text)
import WTP.Formula (Formula, withQueries)
import WTP.Query
import WTP.Specification
import WTP.Verify
import Web.Api.WebDriver hiding (Selector)
import Control.Monad.Freer (Eff)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Either (partitionEithers)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (listToMaybe)
import Data.List (nub)

type WD = WebDriverTT IdentityT IO

run :: Specification Formula (Query ': WD ': '[]) -> WD [Step]
run spec = (:) <$> buildStep <*> traverse (\action -> runAction action >> buildStep) (actions spec)
  where
    extractQueries = withQueries runQuery (property spec)
    runAction = \case
        Focus s -> find1 s >>= elementSendKeys ""
        KeyPress c -> getActiveElement >>= elementSendKeys [c]
        Click s -> find1 s >>= elementClick
        Navigate (Path path) -> navigateTo (Text.unpack path)
    buildStep = do
      values <- Eff.runM extractQueries
      let (queriedElements, elementStates) =
            bimap groupUniqueIntoMap groupUniqueIntoMap (partitionEithers (concat values))
      pure ( Step { queriedElements, elementStates })

find1 :: Selector -> WD ElementRef
find1 (Selector s) = findElement CssSelector (Text.unpack s)

findMaybe :: Selector -> WD (Maybe ElementRef)
findMaybe = fmap listToMaybe . findAll

findAll :: Selector -> WD [ElementRef]
findAll (Selector s) = findElements CssSelector (Text.unpack s)

toRef :: Element -> ElementRef
toRef (Element ref) = ElementRef (Text.unpack ref)

fromRef :: ElementRef -> Element
fromRef (ElementRef ref) = Element (Text.pack ref)

groupUniqueIntoMap :: (Eq a, Hashable a, Eq b) => [(a, b)] -> HashMap a [b]
groupUniqueIntoMap = HashMap.map nub . HashMap.fromListWith (++) . map (\(k, v) -> (k, [v]))

type QueriedElement = (Selector, Element) 
type QueriedElementState = (Element, ElementStateValue)

runQuery
      :: Eff.LastMember WD effs 
      => Eff (Query ': effs) a -> Eff effs [Either QueriedElement QueriedElementState]
runQuery query' =
  fmap snd
    $ runWriter
    $ Eff.reinterpret go query'
  where
    go 
      :: Eff.Member (Writer [Either (Selector, Element) (Element, ElementStateValue)]) effs
      => Eff.LastMember WD effs 
      => Query ~> Eff effs
    go =
      ( \case
          Query selector -> do
            el <- fmap fromRef <$> Eff.sendM (findMaybe selector)
            case el of
              Just el' -> tell [Left (selector, el') :: Either QueriedElement QueriedElementState]
              Nothing -> pure ()
            pure el
          QueryAll selector -> do
            els <- fmap fromRef <$> Eff.sendM (findAll selector)
            tell ((Left . (selector, ) <$> els) :: [Either QueriedElement QueriedElementState])
            pure [Element "a"]
          Get state element -> do
            value <- Eff.sendM $ case state of
              Attribute name -> fmap Text.pack <$> getElementAttribute (Text.unpack name) (toRef element)
              Property name -> getElementProperty (Text.unpack name) (toRef element)
              CssValue name -> Text.pack <$> getElementCssValue (Text.unpack name) (toRef element)
              Text -> Text.pack <$> getElementText (toRef element)
              Enabled -> isElementEnabled (toRef element)
            tell [Right (element, ElementStateValue state value) :: Either QueriedElement QueriedElementState]
            pure value
      )
{-
myWait :: Int -> WebDriverT IO ()
myWait ms =
  void
    ( executeAsyncScript
        " var ms = arguments[0]; \
        \ var done = arguments[1]; \
        \ setTimeout(done, ms) \
        \"
        [JSON.toJSON ms]
    )

-}
