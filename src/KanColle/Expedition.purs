module KanColle.Expedition where

import Prelude
import Data.Array

import KanColle.Expedition.Requirement
import KanColle.Expedition.IncomeBase
import KanColle.Expedition.Cost
import KanColle.Expedition.Base

validExpeditionId :: Int -> Boolean
validExpeditionId = getExpeditionRequirement >>> null >>> not

getAvailableExpeditions :: forall a. Fleet a -> Array Int
getAvailableExpeditions fleet =
  filter (\eId -> checkExpedition eId fleet) allExpeditionIds

checkExpedition :: forall a. Int -> Fleet a -> Boolean
checkExpedition eId fleet = validExpeditionId eId
                         && null (unsatisfiedRequirements eId fleet)

type Expedition =
  { id :: Int
  , req :: ExpeditionRequirement
  , income :: IncomeBase
  , cost :: Cost
  }

getExpeditionInfo :: Int -> Expedition
getExpeditionInfo eId =
    { id: eId
    , req: getExpeditionRequirement eId
    , income: getExpeditionIncomeBase eId
    , cost: getExpeditionCost eId }

allExpeditions :: Array Expedition
allExpeditions = map getExpeditionInfo allExpeditionIds
