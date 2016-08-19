module KanColle.Expedition.New.EArray
  ( EArray()
  , mkEA
  , indEA
  ) where

import Prelude
import Partial.Unsafe
import Partial
import Data.Array as A
import Data.Array.Partial as AP

-- array wrappered for all 40 expeditions
newtype EArray a = EA (Array a)

mkEA :: forall a. Array a -> EArray a
mkEA xs
    | A.length xs == 40 = EA xs
    | otherwise = unsafePartial (crash "expecting exactly 40 elements")

indEA :: forall a. EArray a -> Int -> a
indEA (EA xs) i
    | 1 <= i && i <= 40 = unsafePartial (AP.unsafeIndex xs (i-1))
    | otherwise = unsafePartial (crash "index out of range")
