{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as Text
import Data.Text (Text)
import WTP.Language
import Dhall (input, auto, FromDhall, Generic)
import Data.Text (Text)
import System.Environment (getArgs)

main :: IO ()
main = do
    paths <- getArgs
    specs <- mapM (loadSpecification . Text.pack) paths
    print specs


loadSpecification :: Text -> IO Formula
loadSpecification path = 
  embedFormula <$> input auto path