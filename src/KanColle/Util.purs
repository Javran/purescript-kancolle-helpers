{- | This module stores utility functions
 -}
module KanColle.Util
  ( todo
  , throwWith
  , traceLog
  , traceWarn
  
  , jsonStringify
  
  , peekSTArrayUnsafe
  , pokeSTArrayUnsafe
  
  , times
  , chooseN
  , chooseN_FFI
  
  , unsafeArrIndex
  , unsafeArrHead
  , unsafeArrTail
  
  , LR
  , lrMap
  , memptyLR
  , lrOnlyLeft
  , lrOnlyRight
  , lrAppend
  
  , fleetSplit
  
  , toFixed
  ) where

import Prelude
import Data.Monoid
import Data.Maybe
import Data.Int
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.ST

import Data.Array hiding (length)
import Data.Array.Partial as PA
import Data.Array.ST hiding (peekSTArray, pokeSTArray)
import Data.List as L
import Data.Foldable
import Control.Plus
import Partial.Unsafe
import Data.Function.Uncurried

-- | `JSON.stringify`
foreign import jsonStringify :: forall a. a -> String

foreign import consoleMessage :: forall a b. Int -> a -> (Unit -> b) -> b

-- | throw anything, raise exceptions from pure code
foreign import throwWith :: forall a b. a -> b

unsafeArrIndex :: forall a. Array a -> Int -> a
unsafeArrIndex = unsafePartial unsafeIndex

unsafeArrHead :: forall a. Array a -> a
unsafeArrHead = unsafePartial PA.head

unsafeArrTail :: forall a. Array a -> Array a
unsafeArrTail = unsafePartial PA.tail

-- | use it as a placeholder for any not yet implemented
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

-- | replicate a `Monoid` for a given number of times and `mconcat` the result
times :: forall m. (Monoid m) => Int -> m -> m
times 0 _ = mempty
times n m = times1p (n-1) m

-- | similar to Haskell's `Data.List.tails`
-- | e.g. `tails [1,2,3] = [[1,2,3],[2,3],[3],[]]`
tails :: forall a. L.List a -> L.List (L.List a)
tails xs = case xs of
    L.Nil -> L.Cons L.Nil L.Nil
    L.Cons _ tl -> L.Cons xs (tails tl)

-- | `chooseN xs n` non-deterministically chooses `n` values from `xs`.
-- | It is recommended to use only small values on `n` (0,1,2,3).
chooseN :: forall a f. (Foldable f) => f a -> Int -> Array (L.List a)
chooseN xs = L.toUnfoldable <<< pickFrom xsL
  where
    xsL = L.fromFoldable xs
    pickFrom _ 0 = L.Cons L.Nil L.Nil
    pickFrom remaining i = do
      ys <- tails remaining
      case ys of
        L.Nil -> empty
        L.Cons hd tl -> do
          rs <- pickFrom tl (i-1)
          pure (L.Cons hd rs)
          
chooseN_FFI :: forall a. Fn2 (Array a) Int (Array (Array a))
chooseN_FFI = mkFn2 (\xs n -> L.toUnfoldable <$> chooseN xs n)

-- | `peekSTArray` without array bound checks
foreign import peekSTArrayUnsafe :: forall a h r. STArray h a -> Int -> Eff (st :: ST h | r) a
-- | `pokeSTArray` without array bound checks
foreign import pokeSTArrayUnsafe :: forall a h r. STArray h a -> Int -> a -> Eff (st :: ST h | r) Unit

-- | `console.log` anything
traceLog :: forall a b. a -> (Unit -> b) -> b
traceLog = consoleMessage 0

-- | `console.warn` anything
traceWarn :: forall a b. a -> (Unit -> b) -> b
traceWarn = consoleMessage 1

-- | something that has both "left" part and "right" part
type LR a =
  { left :: a
  , right :: a
  }

-- | `fleetSplit cutHead xs` cuts `xs` into two 6-element arrays.
-- | `xs` has to be of length 12 (or 13 when `cutHead` is true)
-- | if `cutHead` is true, the first value of `xs` is dropped before cutting.
fleetSplit :: forall a. Boolean -> Array a -> LR (Array a)
fleetSplit cutHead xs = if check
    then { left: slice 0 6 ys
         , right: slice 6 12 ys }
    else throwWith "fleetSplit: array length need to be 12 (or 13 on raw)"
  where
    ys = if cutHead
           then (unsafePartial PA.tail xs)
           else xs
    check = length ys == 12

-- | apply a function to both parts of an `LR`
lrMap :: forall a b. (a -> b) -> LR a -> LR b
lrMap f x = { left: f x.left, right: f x.right }

-- | `mempty` for `LR`
memptyLR :: forall m. Monoid m => LR m
memptyLR = {left: mempty, right: mempty}

-- | `append` for `LR`
lrAppend :: forall m. Monoid m => LR m -> LR m -> LR m
lrAppend a b = { left: a.left <> b.left, right: a.right <> b.right }

-- | lift some Monoid that has only left part into `LR`
lrOnlyLeft :: forall m. Monoid m => m -> LR m
lrOnlyLeft l = { left: l, right: mempty }

-- | lift some Monoid that has only right part into `LR`
lrOnlyRight :: forall m. Monoid m => m -> LR m
lrOnlyRight r = { left: mempty, right: r }

foreign import unsafeToFixed :: forall eff. Int -> Number -> Eff eff String

-- from: https://github.com/Jonplussed/purescript-number-format
toFixed :: Int -> Number -> Maybe String
toFixed scale num = runPure (catchException (pure <<< const Nothing) (Just <$> unsafeToFixed scale num))
