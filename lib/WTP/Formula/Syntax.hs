{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module WTP.Formula.Syntax
  ( WTP.Formula,
    WTP.Proposition,
    Lattice(..), 
    Heyting(..), 
    (===),
    not,
  )
where

import qualified WTP.Formula.Logic as WTP
import Algebra.Lattice
import Algebra.Heyting

(===) :: WTP.Formula a -> WTP.Formula a -> WTP.Proposition
(===) = WTP.Equals