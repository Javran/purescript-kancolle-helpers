module KanColle.SType
  ( module KanColle.Generated.SType
  ) where

import Prelude
import KanColle.Generated.SType

instance eqSType :: Eq SType where
  eq a b = showSType a == showSType b
