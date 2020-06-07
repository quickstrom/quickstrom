{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module WebCheck.Specification where

import WebCheck.Element
import WebCheck.Trace
import WebCheck.Path
import WebCheck.Query
import WebCheck.Result
import Test.QuickCheck (Gen)
import Data.Text.Prettyprint.Doc (Doc)
import Data.HashSet (HashSet)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

type ActionGenerator = Gen (Action Selector)

class Specification s where
  origin :: s -> Path
  readyWhen :: s -> Selector
  actions :: s -> ActionGenerator
  queries :: s -> HashSet Query
  verify :: s -> [ObservedState] -> Either (Doc AnsiStyle) Result
