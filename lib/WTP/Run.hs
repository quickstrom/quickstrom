{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module WTP.Run (run) where

import Control.Monad (void)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Aeson as JSON
import Web.Api.WebDriver
import WTP.Specification
import WTP.Verify
import WTP.Formula
import WTP.Query

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
    pure
      (Step
        { queriedElements = mempty
        , elementStates   =
          [ ( Element "a"
            , [ SomeElementState (Property "classList")
                                 (JSON.Array [JSON.String "foo"])
              ]
            )
          ]
        }
      )

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
