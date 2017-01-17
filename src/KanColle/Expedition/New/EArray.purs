module KanColle.Expedition.New.EArray
  ( EArray()
  , mkEA
  , indEA
  , imapEA
  , unEA
  , extractEA
  ) where

import Prelude
import Partial.Unsafe
import Partial
import Data.Array as A
import Data.Traversable
import Data.Unfoldable
import Data.Tuple

-- array wrappered for all 40 expeditions
newtype EArray a = EA (Array a)

allExpeds :: Array Int
allExpeds = A.range 1 40


mkEA :: forall a. Array a -> EArray a
mkEA xs
    | A.length xs == 40 = EA xs
    | otherwise = unsafePartial (crash "expecting exactly 40 elements")

indEA :: forall a. EArray a -> Int -> a
indEA (EA xs) i
    | 1 <= i && i <= 40 = unsafePartial (A.unsafeIndex xs (i-1))
    | otherwise = unsafePartial (crash "index out of range")
    
imapEA :: forall a b. (Int -> a -> b) -> EArray a -> EArray b
imapEA f (EA xs) = EA (A.zipWith f allExpeds xs)

unEA :: forall a. EArray a -> Array a
unEA (EA xs) = xs

-- is functor instance necessary?
instance functorEArray :: Functor EArray where
  map f (EA xs) = EA (map f xs)

pureEA :: forall a. a -> EArray a
pureEA =
    -- no need for passing through the smart constructor
    -- at the cost of being more careful about our code
    replicate 40 >>> EA

appEA :: forall a b. EArray (a -> b) -> EArray a -> EArray b
appEA (EA fs) (EA xs) = EA (A.zipWith ($) fs xs)

-- TODO: avoid using this frequently: we don't have to compose arrays
-- instead we can keep origin array and do random access as needed.
extractEA :: forall a. EArray Boolean -> EArray a -> Array a
extractEA (EA bs) (EA xs) = map snd (A.filter fst (A.zip bs xs))

instance applyEArray :: Apply EArray where
  apply = appEA

instance applicativeEArray :: Applicative EArray where
  pure = pureEA

instance foldableEArray :: Foldable EArray where
  foldr f z (EA xs) = foldr f z xs
  foldl f z (EA xs) = foldl f z xs
  foldMap f (EA xs) = foldMap f xs

instance traversableEArray :: Traversable EArray where
  traverse f (EA xs) = EA <$> traverse f xs
  sequence (EA ms) = EA <$> sequence ms
