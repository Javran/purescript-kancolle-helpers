module KanColle.Expedition.Evaluate where

import Prelude
import Data.Array
import qualified Data.String as S
import Data.Int
import Math
import Data.Function

import KanColle.Expedition
import KanColle.Expedition.Income
import KanColle.Expedition.Minimal
import KanColle.Expedition.Cost

type EvalResult =
  { eId :: Int
  , netIncome :: Income
  , score :: Number
  , time :: Int
  }

incomeDiff :: Income -> ECost -> Income
incomeDiff (Income i) (ECost e) = case e.shipCost of
    ShipCost sc -> Income
        { fuel: i.fuel - sc.fuel
        , ammo: i.ammo - sc.ammo
        , steel: i.steel
        , bauxite: i.bauxite
        }

ordMax :: forall a. (Ord a) => a -> a -> a
ordMax a b = if a >= b then a else b

minToHour :: Int -> Number
minToHour m = toNumber m / 60.0

comparing :: forall a b. (Ord a) => (b -> a) -> b -> b -> Ordering
comparing prj x y = prj x `compare` prj y

sortByHourlyGain :: (Income -> Int) -> Array EvalResult
sortByHourlyGain evalCost = sortBy (flip compareScore) expeditions
  where
    compareScore = comparing (\x -> x.score)
    expeditions :: Array EvalResult
    expeditions = map collectInfo expeditionIds
    collectInfo eId = { eId: eId
                      , netIncome: netIncome
                      , time: cost.time
                      , score: score }
      where
        netIncome = getExpeditionIncome eId `incomeDiff` getExpeditionMinCost eId
        cost = getExpeditionCost eId
        score = toNumber (evalCost netIncome) / minToHour cost.time

sortWithAfkTime :: (Income -> Int) -> Number -> Int -> Array EvalResult
sortWithAfkTime evalCost timePenalty afkMinutes = sortBy (flip compareScore) expeditions
  where
    compareScore = comparing (\x -> x.score)
    expeditions :: Array EvalResult
    expeditions = map collectInfo expeditionIds
    collectInfo eId = { eId: eId
                      , netIncome: netIncome
                      , time: totalExpTime
                      , score: score }
      where
        costMinutes = (getExpeditionCost eId).time
        netIncome = getExpeditionIncome eId `incomeDiff` getExpeditionMinCost eId
        totalExpTime = ordMax costMinutes afkMinutes
        scoreNoPenalty = toNumber (evalCost netIncome) / minToHour totalExpTime
        timeDiffPenalty :: Number
        timeDiffPenalty = timePenalty * toNumber (ordMax 0 (costMinutes - afkMinutes))
        score = scoreNoPenalty - timeDiffPenalty

showEvalResult :: EvalResult -> String
showEvalResult er = case er.netIncome of
    Income inc ->
      "Expedition #" <> show er.eId <> ": "
                     <> showHourly inc.fuel <> " | "
                     <> showHourly inc.ammo <> " | "
                     <> showHourly inc.steel <> " | "
                     <> showHourly inc.bauxite <> " | "
                     <> show er.score
  where
    -- TODO
    showHourly v = show (toNumber v / (toNumber (getExpeditionCost er.eId).time / 60.0))

evalResultToJS :: EvalResult -> {eId :: Int, result :: Array Number}
evalResultToJS er = case er.netIncome of
    Income inc ->
      { eId: er.eId
      , result: [ hourly inc.fuel
                , hourly inc.ammo
                , hourly inc.steel
                , hourly inc.bauxite
                , er.score
                ] }
  where
    hourly v = (toNumber v / minToHour er.time)

simpleEvalCost :: (Int -> Int -> Int -> Int -> Int) -> Income -> Int
simpleEvalCost f (Income i) = f i.fuel i.ammo i.steel i.bauxite

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
