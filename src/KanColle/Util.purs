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
import Data.Array.ST hiding (peekSTArray, pokeSTArray)
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
import Data.Foldable
import Data.Monoid
import Control.Plus

foreign import jsonStringify :: forall a. a -> String
foreign import consoleMessage :: forall a b. Int -> a -> (Unit -> b) -> b
foreign import throwWith :: forall a b. a -> b

todo :: forall a b . b -> a
todo _ = throwWith "TODO"

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

tails :: forall a. L.List a -> L.List (L.List a)
tails xs = case xs of
    L.Nil -> L.Cons L.Nil L.Nil
    L.Cons _ tl -> L.Cons xs (tails tl)

-- only valid values are 0,1,2,3
-- TODO: change doc
chooseN :: forall a f. (Foldable f) => f a -> Int -> Array (L.List a)
chooseN xs = L.fromList <<< pickFrom xsL
  where
    xsL = L.toList xs
    pickFrom _ 0 = L.Cons L.Nil L.Nil
    pickFrom remaining i = do
      ys <- tails remaining
      case ys of
        L.Nil -> empty
        L.Cons hd tl -> do
          rs <- pickFrom tl (i-1)
          return (L.Cons hd rs)

foreign import peekSTArrayUnsafe :: forall a h r. STArray h a -> Int -> Eff (st :: ST h | r) a
foreign import pokeSTArrayUnsafe :: forall a h r. STArray h a -> Int -> a -> Eff (st :: ST h | r) Unit

heapify :: forall a h r. (a -> a -> Ordering) -> Array a -> Eff (st :: ST h | r) (STArray h a)
heapify cmp src = do
    hp <- thaw src
    let n = length src
    traverse (startHeapifyDown cmp hp n) ((n/2-1) .. 0)
    return hp

startHeapifyDown :: forall a h r. (a -> a -> Ordering) -> STArray h a -> Int -> Int -> Eff (st :: ST h | r) Unit
startHeapifyDown cmp hp heapSize ind = peekSTArrayUnsafe hp ind >>= heapifyDown cmp hp heapSize ind

heapifyDown :: forall a h r. (a -> a -> Ordering) -> STArray h a -> Int -> Int -> a -> Eff (st :: ST h | r) Unit
heapifyDown cmp hp heapSize = heapifyDown'
  where
    heapifyDown' curInd curV = do
      let lcInd = 2*curInd + 1
          rcInd = 2*curInd + 2
          checkValue ind | ind >= heapSize = return Nothing
          checkValue ind = do
            v <- peekSTArrayUnsafe hp ind
            return (if cmp v curV == GT then Just v else Nothing)
      lcV <- checkValue lcInd
      rcV <- checkValue rcInd
      let downToBranch bV bInd = do
              pokeSTArrayUnsafe hp curInd bV
              pokeSTArrayUnsafe hp bInd curV
              heapifyDown' bInd curV
      case rcV of
        Just rV -> case lcV of
          Just lV -> do
            if cmp lV rV == EQ || cmp lV rV == GT
              then downToBranch lV lcInd
              else downToBranch rV rcInd
          Nothing -> downToBranch rV rcInd
        Nothing -> case lcV of
          Just lV -> downToBranch lV lcInd
          Nothing -> return unit

deleteMax :: forall a h r. (a -> a -> Ordering) -> STArray h a -> Int -> Eff (st :: ST h | r) a
deleteMax cmp hp heapSize = do
  let lastInd = heapSize - 1
  vMax <- peekSTArrayUnsafe hp 0
  vLast <- peekSTArrayUnsafe hp lastInd
  pokeSTArrayUnsafe hp 0 vLast
  pokeSTArrayUnsafe hp lastInd vMax
  startHeapifyDown cmp hp (heapSize-1) 0
  return vMax

heapSort :: forall a. (a -> a -> Ordering) -> Array a -> Array a
heapSort cmp arr = runPure (runSTArray (do
    let n = length arr
    hp <- heapify cmp arr
    traverse (deleteMax cmp hp) (n..2)
    return hp))

heapSortSafe :: forall a. (a -> a -> Ordering) -> Array a -> Array a
heapSortSafe cmp xs
  | length xs < 2 = xs
  | otherwise = heapSort cmp xs

sortByThenTake :: forall a. (a -> a -> Ordering) -> Int -> Array a -> Array a
sortByThenTake cmp n = sortBy cmp >>> take n

sortByThenTakeQuick :: forall a. (a -> a -> Ordering) -> Int -> Array a -> Array a
sortByThenTakeQuick cmp n xs =
    if l < 2 || n <= 0
      then (sortBy cmp >>> take n) xs
      else if n >= l
        then sortBy cmp xs
        else runPure (runST (do
          hp <- heapify fcmp xs
          traverse (deleteMax fcmp hp) (l .. (l-n+1))))
  where
    fcmp = flip cmp
    l = length xs

traceLog :: forall a b. a -> (Unit -> b) -> b
traceLog = consoleMessage 0

traceWarn :: forall a b. a -> (Unit -> b) -> b
traceWarn = consoleMessage 1
