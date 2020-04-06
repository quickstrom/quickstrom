{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module WTP.Run (run) where

import Control.Monad (void)
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Aeson as JSON
import Web.Api.WebDriver hiding (Selector)
import WTP.Specification
import WTP.Verify
import WTP.Formula
import WTP.Query
import Control.Monad.Freer (interpretM, reinterpret, interpret, Eff)
import qualified Control.Monad.Freer as Eff
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import qualified Data.Vector as Vector
import Control.Monad.Trans.Identity (IdentityT)

run :: Specification Formula -> WebDriverT IO [Step]
run spec = traverse go (actions spec)
 where
  find' (Selector s) = findElement CssSelector (Text.unpack s)
  go action = do
    case action of
      Focus    s           -> find' s >>= elementSendKeys ""
      KeyPress c           -> getActiveElement >>= elementSendKeys [c]
      Click    s           -> find' s >>= elementClick
      Navigate (Path path) -> navigateTo (Text.unpack path)
    Eff.runM (runError (withQueries runQuery (property spec)))
    pure
      (Step
        { queriedElements = mempty
        , elementStates   =
          HashMap.fromList
            [ ( Element "a"
              , [ ElementStateValue (Property "classList")
                                    (JSON.Array (Vector.singleton (JSON.String "foo")))
                ]
              )
            ]
        }
      )

toRef :: Element -> ElementRef
toRef (Element ref) = ElementRef (Text.unpack ref)

runQuery ::
  -- Eff.LastMember (WebDriverTT IdentityT IO) effs =>
  Eff '[Query, Error Text] a ->
  Eff '[Error Text, WebDriverTT IdentityT IO] [(Element, ElementStateValue)]
runQuery =
  fmap snd
    . runWriter
    . reinterpret go
  where
    go ::
      Eff.Member (Writer [(Element, ElementStateValue)]) effs =>
      Eff.LastMember (WebDriverTT IdentityT IO) effs =>
      Query a ->
      Eff effs a
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