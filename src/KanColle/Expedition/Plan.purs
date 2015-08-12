module KanColle.Expedition.Plan where

import Prelude
import KanColle.Expedition.Evaluate
import KanColle.Expedition.Income
import KanColle.Expedition.Cost
import KanColle.Expedition.Minimal
import KanColle.Expedition.NetIncome
import KanColle.Expedition
import Control.Plus
import qualified Data.Array as A
import qualified Data.List as L
import Data.Foldable
import Data.Traversable
import Data.Function
import Data.String (joinWith)
import Number.Format
import Data.Maybe
import Math (sqrt)

import Control.Monad.Eff.Console
import Control.Monad.Eff

tails :: forall a. L.List a -> L.List (L.List a)
tails xs = case xs of
    L.Nil -> L.Cons L.Nil L.Nil
    L.Cons _ tl -> L.Cons xs (tails tl)

-- only valid values are 0,1,2,3
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

-- with resource priority and afktime
calcNetIncome :: Number
              -> Number
              -> Number
              -> Number
              -> Number
              -> Int
              -> Array { eIds :: Array Int, hourly :: HourlyIncome, resourceScore :: Number }
calcNetIncome alp pF pA pS pB afkMins = A.sortBy (flip compare `on` (\x -> x.resourceScore)) hourlyNetIncomeTable
  where
    hourlyNetIncomeTable = map (calcResourceScore <<< mergeHNetIncome) $ netIncomeWithAfkTime afkMins `chooseN` 3
    calcResourceScore :: { eIds :: Array Int, hourly :: HourlyIncome }
                      -> { eIds :: Array Int, hourly :: HourlyIncome, resourceScore :: Number }
    calcResourceScore x = { eIds: x.eIds, hourly: x.hourly, resourceScore: score - rPenalty * alp }
      where
        rPenalty = ratioPenalty [pF,pA,pS,pB] [x.hourly.fuel, x.hourly.ammo, x.hourly.steel, x.hourly.bauxite]
        score = x.hourly.fuel * pF
              + x.hourly.ammo * pA
              + x.hourly.steel * pS
              + x.hourly.bauxite * pB

showNI :: { eIds :: Array Int, hourly :: HourlyIncome, resourceScore :: Number } -> String
showNI x = joinWith " | " [ show x.eIds
                          , f x.hourly.fuel
                          , f x.hourly.ammo
                          , f x.hourly.steel
                          , f x.hourly.bauxite
                          , f x.resourceScore ]
  where
    f y = fromMaybe "ERR" (toFixed 3 y)

dbg :: Number
    -> Number
    -> Number
    -> Number
    -> Int
    -> Number
    -> Eff (console :: CONSOLE) Unit
dbg pF pA pS pB atime a = do
    void $ traverse (showNI >>> log) $ A.take 50 $ calcNetIncome a pF pA pS pB atime
    return unit

ratioPenalty :: Array Number -> Array Number -> Number
ratioPenalty expect actual = scoresSq
  where
    expectSum = sum expect
    expectR = map (/ expectSum) expect
    actualSum = sum (A.filter (>= 0.0) actual)
    actualR = map (/ actualSum) actual
    scoresSq = sqrt $ sum $ A.zipWith (\x y -> (x-y)*(x-y)) expectR actualR
