{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module WebCheck.Specification where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Vector (Vector)
import WebCheck.Element
import WebCheck.Prelude
import WebCheck.Result
import WebCheck.Trace

type Queries = HashMap Selector (HashSet ElementState)

class Specification s where
  readyWhen :: s -> Selector
  actions :: s -> Vector (Int, Action Selector)
  queries :: s -> Queries
  verify :: s -> [ObservedState] -> Either Text Result
