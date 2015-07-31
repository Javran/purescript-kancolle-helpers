module KanColle.Util where

import Prelude
import Data.Monoid
import Data.Int

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
