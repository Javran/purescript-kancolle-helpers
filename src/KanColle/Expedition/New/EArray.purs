module KanColle.Expedition.New.EArray
  ( EArray()
  , mkEA
  , indEA
  , unEA
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

unEA :: forall a. EArray a -> Array a
unEA (EA xs) = xs

-- is functor instance necessary?
instance functorEArray :: Functor EArray where
  map f (EA xs) = EA (map f xs)
