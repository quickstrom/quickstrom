{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
module WTP.PureScript.Foreign where

import Protolude
import Language.PureScript.Names (Ident, Qualified)

data ApplyForeign = ApplyForeign (Qualified Ident) [Ident]
  deriving (Show, Generic)
