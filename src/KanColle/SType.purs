module KanColle.SType
  ( module KanColle.Generated.SType
  , eqSType
  ) where

import Prelude
import KanColle.Generated.SType

eqSType :: SType -> SType -> Boolean
eqSType a b = showSType a == showSType b
