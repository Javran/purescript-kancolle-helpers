module DamageVectorTests where

import Prelude

import KanColle.DamageAnalysis.DamageVector
import qualified Data.String as Str
import qualified Data.Array.Unsafe as AU

import Base

-- | convert a DamageVector to a simple string form
dvToStr :: DamageVector -> String
dvToStr (DV dv) = Str.joinWith ","
              <<< map show
              <<< AU.tail
                $ dv
