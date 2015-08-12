module KanColle.Expedition.NetIncome where

import Prelude
import KanColle.Expedition.Evaluate
import KanColle.Expedition.Income
import KanColle.Expedition.Cost
import KanColle.Expedition.Minimal
import KanColle.Expedition
import Data.Array
import Data.Maybe
import Data.Monoid
import Number.Format
import Data.String (joinWith)
import Data.Int
import Data.Foldable

type NetIncome =
  { eId :: Int
  , netIncome :: Income
  }

type HourlyIncome =
  { fuel :: Number
  , ammo :: Number
  , steel :: Number
  , bauxite :: Number
  }

type ExpeNetIncomeHourly =
  { eId :: Int
  , hourly :: HourlyIncome
  }

mergeHNetIncome :: forall f. (Functor f, Foldable f)
                => f ExpeNetIncomeHourly
                -> { eIds :: Array Int
                   , hourly :: HourlyIncome
                   }
mergeHNetIncome xs = { eIds: foldMap (\x -> [x.eId]) xs
                     , hourly: { fuel:    sum (map (\x -> x.hourly.fuel) xs)
                               , ammo:    sum (map (\x -> x.hourly.ammo) xs)
                               , steel:   sum (map (\x -> x.hourly.steel) xs)
                               , bauxite: sum (map (\x -> x.hourly.bauxite) xs)
                               } }

netIncomeTable :: Array NetIncome
netIncomeTable = map collectInfo expeditionIds
  where
    collectInfo eId = { eId: eId
                      , netIncome: netIncome
                      }
      where
        cost = getExpeditionCost eId
        minCost = getExpeditionMinCost eId
        eIncome = getExpeditionIncome eId
        netIncome = eIncome `incomeDiff cost` minCost

showNetIncome :: NetIncome -> String
showNetIncome ni = case ni.netIncome of
    Income i -> joinWith " | " [show ni.eId, s i.fuel, s i.ammo, s i.steel, s i.bauxite]
  where
    s = show

netIncomeWithAfkTime :: Int -> Array ExpeNetIncomeHourly
netIncomeWithAfkTime atime = map transform netIncomeTable
  where
    transform ni = case ni.netIncome of
        Income i -> { eId: ni.eId
                    , hourly: { fuel: t i.fuel
                              , ammo: t i.ammo
                              , steel: t i.steel
                              , bauxite: t i.bauxite } }
      where
        t resource = toNumber resource / (toNumber totalTime / 60.0)
        totalTime = ordMax (getExpeditionCost ni.eId).time atime

showNetHourlyIncome :: ExpeNetIncomeHourly -> String
showNetHourlyIncome ni = joinWith " | " [show ni.eId, s i.fuel, s i.ammo, s i.steel, s i.bauxite]
  where
    i = ni.hourly
    s x = fromMaybe "ERR" (toFixed 3 x)