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
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Natural (type (~>))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Vector as Vector
import WTP.Formula
import WTP.Query
import WTP.Specification
import WTP.Verify
import Web.Api.WebDriver hiding (Selector)
import Control.Monad.Freer (Eff)

run :: Specification Formula -> WebDriverT IO [Step]
run spec = traverse go (actions spec)
  where
    find' (Selector s) = findElement CssSelector (Text.unpack s)
    go action = do
      case action of
        Focus s -> find' s >>= elementSendKeys ""
        KeyPress c -> getActiveElement >>= elementSendKeys [c]
        Click s -> find' s >>= elementClick
        Navigate (Path path) -> navigateTo (Text.unpack path)
      (r :: Either Text [(Element, ElementStateValue)]) <- Eff.runM (runError (runQuery _))
      (r :: Either Text [[(Element, ElementStateValue)]]) <- Eff.runM (runError (withQueries (runQuery . _) (property spec)))
      pure
        ( Step
            { queriedElements = mempty,
              elementStates =
                HashMap.fromList
                  [ ( Element "a",
                      [ ElementStateValue
                          (Property "classList")
                          (JSON.Array (Vector.singleton (JSON.String "foo")))
                      ]
                    )
                  ]
            }
        )

toRef :: Element -> ElementRef
toRef (Element ref) = ElementRef (Text.unpack ref)

runQuery
      :: Eff.LastMember (WebDriverTT IdentityT IO) effs 
      => Eff (Query ': effs) a -> Eff effs [(Element, ElementStateValue)]
runQuery query' =
  fmap snd
    $ runWriter
    $ Eff.reinterpret go query'
  where
    go 
      :: Eff.Member (Writer [(Element, ElementStateValue)]) effs
      => Eff.LastMember (WebDriverTT IdentityT IO) effs 
      => Query ~> Eff effs
    go =
      ( \case
          Query selector -> pure (Just (Element "a"))
          QueryAll selector -> do
            pure [Element "a"]
          Get state element -> do
            value <- Eff.sendM $ case state of
              Attribute name -> fmap Text.pack <$> getElementAttribute (Text.unpack name) (toRef element)
              Property name -> getElementProperty (Text.unpack name) (toRef element)
              CssValue name -> Text.pack <$> getElementCssValue (Text.unpack name) (toRef element)
              Text -> Text.pack <$> getElementText (toRef element)
              Enabled -> isElementEnabled (toRef element)
            tell [(element, ElementStateValue state value)]
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
