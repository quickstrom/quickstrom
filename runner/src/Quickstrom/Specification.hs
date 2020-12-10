{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Quickstrom.Specification where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Vector (Vector)
import Quickstrom.Action
import Quickstrom.Element
import Quickstrom.Prelude
import Quickstrom.Result
import Quickstrom.Trace

type Queries = HashMap Selector (HashSet ElementState)

class Specification s where
  readyWhen :: s -> Selector
  actions :: s -> Vector (Int, PotentialAction)
  queries :: s -> Queries
  verify :: s -> [ObservedState] -> Either Text Result
