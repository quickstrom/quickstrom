module WebCheck.TH where

import Data.FileEmbed
import Language.Haskell.TH.Syntax (qAddDependentFile, Q, Exp)

embedStringFile' :: FilePath -> Q Exp
embedStringFile' path = do
  qAddDependentFile path
  embedStringFile path
