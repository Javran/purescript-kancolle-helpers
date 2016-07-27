module KanColle.Expedition.Evaluate where

import Prelude
import Data.Array
import Data.Int
import Data.Function.Uncurried
import Data.Foldable

import KanColle.Expedition.Base
import KanColle.Expedition.IncomeBase
import KanColle.Expedition.Minimal
import KanColle.Expedition.Cost

type EvalResult =
  { eId :: Int
  , netIncome :: IncomeBase
  , score :: Number
  , time :: Int
  }

nToFloor :: Number -> Int
nToFloor = floor

incomeDiff :: Cost -> IncomeBase -> ECost -> IncomeBase
incomeDiff cost iv (ECost e) = mkIncomeBase
    { fuel: i.fuel - sum fuelCosts
    , ammo: i.ammo - sum ammoCosts
    , steel: i.steel
    , bauxite: i.bauxite
    }
  where
    i = getIncomeBase iv
    sc = e.shipCost

    fuelCostPercent = cost.fuel
    ammoCostPercent = cost.ammo

    fuelCosts :: Array Int
    fuelCosts = map (\x -> nToFloor (toNumber x.fuel * fuelCostPercent)) sc
    ammoCosts :: Array Int
    ammoCosts = map (\x -> nToFloor (toNumber x.ammo * ammoCostPercent)) sc

ordMax :: forall a. (Ord a) => a -> a -> a
ordMax a b = if a >= b then a else b

minToHour :: Int -> Number
minToHour m = toNumber m / 60.0

-- comparing :: forall a b. (Ord a) => (b -> a) -> b -> b -> Ordering
-- comparing prj x y = prj x `compare` prj y

sortByHourlyGain :: (IncomeBase -> Int) -> Array EvalResult
sortByHourlyGain evalCost = sortBy (flip compareScore) expeditions
  where
    compareScore = comparing (\x -> x.score)
    expeditions :: Array EvalResult
    expeditions = map collectInfo allExpeditionIds
    collectInfo eId = { eId: eId
                      , netIncome: netIncome
                      , time: cost.time
                      , score: score }
      where
        netIncome = getExpeditionIncomeBase eId `incomeDiff cost` getExpeditionMinCost eId
        cost = getExpeditionCost eId
        score = toNumber (evalCost netIncome) / minToHour cost.time

sortWithAfkTime :: (IncomeBase -> Int) -> Number -> Int -> Array EvalResult
sortWithAfkTime evalCost timePenalty afkMinutes = sortBy (flip compareScore) expeditions
  where
    compareScore = comparing (\x -> x.score)
    expeditions :: Array EvalResult
    expeditions = map collectInfo allExpeditionIds
    collectInfo eId = { eId: eId
                      , netIncome: netIncome
                      , time: totalExpTime
                      , score: score }
      where
        cost = getExpeditionCost eId
        costMinutes = cost.time
        netIncome = getExpeditionIncomeBase eId `incomeDiff cost` getExpeditionMinCost eId
        totalExpTime = ordMax costMinutes afkMinutes
        scoreNoPenalty = toNumber (evalCost netIncome) / minToHour totalExpTime
        timeDiffPenalty :: Number
        timeDiffPenalty = timePenalty * toNumber (ordMax 0 (costMinutes - afkMinutes))
        score = scoreNoPenalty - timeDiffPenalty

showEvalResult :: EvalResult -> String
showEvalResult er =
    "Expedition #" <> show er.eId <> ": "
                   <> showHourly inc.fuel <> " | "
                   <> showHourly inc.ammo <> " | "
                   <> showHourly inc.steel <> " | "
                   <> showHourly inc.bauxite <> " | "
                   <> show er.score
  where
    -- TODO
    inc = getIncomeBase er.netIncome
    showHourly v = show (toNumber v / (toNumber (getExpeditionCost er.eId).time / 60.0))

evalResultToJS :: EvalResult -> {eId :: Int, result :: Array Number}
evalResultToJS er =
      { eId: er.eId
      , result: [ hourly inc.fuel
                , hourly inc.ammo
                , hourly inc.steel
                , hourly inc.bauxite
                , er.score
                ] }
  where
    inc = getIncomeBase er.netIncome
    hourly v = (toNumber v / minToHour er.time)

simpleEvalCost :: (Int -> Int -> Int -> Int -> Int) -> IncomeBase -> Int
simpleEvalCost f income = f i.fuel i.ammo i.steel i.bauxite
  where
    i = getIncomeBase income

evalNetIncomeHourlyJS :: (Fn4 Int Int Int Int Int) -> Array {eId:: Int, result :: Array Number}
evalNetIncomeHourlyJS sEvalCost = map evalResultToJS $
    sortByHourlyGain (simpleEvalCost (runFn4 sEvalCost))

evalNetIncomeWithAfkMinutesJS' :: (Fn4 Int Int Int Int Int) -> Number -> Int
                              -> Array {eId:: Int, result :: Array Number}
evalNetIncomeWithAfkMinutesJS' sEvalCost penalty afkMinutes = map evalResultToJS $
    sortWithAfkTime (simpleEvalCost (runFn4 sEvalCost)) penalty afkMinutes

evalNetIncomeWithAfkMinutesJS :: Fn3 (Fn4 Int Int Int Int Int)
                                     Number
                                     Int
                                     (Array {eId:: Int, result :: Array Number})
evalNetIncomeWithAfkMinutesJS = mkFn3 evalNetIncomeWithAfkMinutesJS'
