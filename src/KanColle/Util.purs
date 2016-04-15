module KanColle.Util where

import Prelude
import Data.Monoid
import Data.Int
import Control.Monad.Eff
import Control.Monad.ST
import Data.Traversable

import Data.Array
import Data.Array.Unsafe as AU
import Data.Array.ST hiding (peekSTArray, pokeSTArray)
import Data.Maybe
import Data.List as L
import Data.Foldable
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

traceLog :: forall a b. a -> (Unit -> b) -> b
traceLog = consoleMessage 0

traceWarn :: forall a b. a -> (Unit -> b) -> b
traceWarn = consoleMessage 1

type LR a =
  { left :: a
  , right :: a
  }

fleetSplit :: forall a. Boolean -> Array a -> LR (Array a)
fleetSplit cutHead xs = if check
    then { left: slice 0 6 ys
         , right: slice 6 12 ys }
    else throwWith "fleetSplit: array length need to be 12 (or 13 on raw)"
  where
    ys = if cutHead
           then AU.tail xs
           else xs
    check = length ys == 12

lrMap :: forall a b. (a -> b) -> LR a -> LR b
lrMap f x = { left: f x.left, right: f x.right }

memptyLR :: forall m. Monoid m => LR m
memptyLR = {left: mempty, right: mempty}

lrAppend :: forall m. Monoid m => LR m -> LR m -> LR m
lrAppend a b = { left: a.left <> b.left, right: a.right <> b.right }

lrOnlyLeft :: forall m. Monoid m => m -> LR m
lrOnlyLeft l = { left: l, right: mempty }

lrOnlyRight :: forall m. Monoid m => m -> LR m
lrOnlyRight r = { left: mempty, right: r }
