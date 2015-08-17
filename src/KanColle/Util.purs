module KanColle.Util where

import Prelude
import Data.Monoid
import Data.Int
import Control.Monad.Eff
import Control.Monad.ST
import Control.MonadPlus
import Control.Apply
import Data.Traversable

import Data.Maybe.Unsafe
import Data.Array
import Data.Array.ST
import Data.Monoid
import Data.Maybe

times1p :: forall m. (Semigroup m) => Int -> m -> m
times1p y0 x0 = f x0 (y0 + 1)
  where
    f x y
      | even y = f (x <> x) (y / 2)
      | y == 1 = x
      | otherwise = g (x <> x) ((y - 1) / 2) x
    g x y z
      | even y = g (x <> x) (y / 2) z
      | y == 1 = x <> z
      | otherwise = g (x <> x) ((y - 1) / 2) (x <> z)

times :: forall m. (Monoid m) => Int -> m -> m
times 0 _ = mempty
times n m = times1p (n-1) m

heapify :: forall a h r. (Ord a) => Array a -> Eff (st :: ST h | r) (STArray h a)
heapify src = do
    hp <- thaw src
    let n = length src
    traverse (startHeapifyDown hp n) ((n/2-1) .. 0)
    return hp

startHeapifyDown :: forall a h r. (Ord a) => STArray h a -> Int -> Int -> Eff (st :: ST h | r) Unit
startHeapifyDown hp heapSize ind = fromJust <$> peekSTArray hp ind >>= heapifyDown hp heapSize ind

heapifyDown :: forall a h r. (Ord a) => STArray h a -> Int -> Int -> a -> Eff (st :: ST h | r) Unit
heapifyDown hp heapSize curInd curV = do
    let lcInd = 2*curInd + 1
        rcInd = 2*curInd + 2
        checkValue ind | ind >= heapSize = return Nothing
        checkValue ind = do
          v <- fromJust <$> peekSTArray hp ind
          return (if v > curV then Just v else Nothing)
    lcV <- checkValue lcInd
    rcV <- checkValue rcInd
    let downToBranch bV bInd = do
            pokeSTArray hp curInd bV
            pokeSTArray hp bInd curV
            heapifyDown hp heapSize bInd curV
    case rcV of
      Just rV -> case lcV of
        Just lV -> do
          if lV >= rV
            then downToBranch lV lcInd
            else downToBranch rV rcInd
        Nothing -> downToBranch rV rcInd
      Nothing -> case lcV of
        Just lV -> downToBranch lV lcInd
        Nothing -> return unit

deleteMax :: forall a h r. (Ord a) => STArray h a -> Int -> Eff (st :: ST h | r) a
deleteMax hp heapSize = do
  let lastInd = heapSize - 1
  vMax <- fromJust <$> peekSTArray hp 0
  vLast <- fromJust <$> peekSTArray hp lastInd
  pokeSTArray hp 0 vLast
  pokeSTArray hp lastInd vMax
  startHeapifyDown hp (heapSize-1) 0
  return vMax

heapSort :: forall a. (Ord a) => Array a -> Array a
heapSort arr = runPure (runSTArray (do
    let n = length arr
    hp <- heapify arr
    traverse (deleteMax hp) (n..2)
    return hp))

heapSortSafe :: forall a. (Ord a) => Array a -> Array a
heapSortSafe xs
  | length xs < 2 = xs
  | otherwise = heapSort xs
