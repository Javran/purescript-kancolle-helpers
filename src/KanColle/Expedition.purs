module KanColle.Expedition where

import Prelude
import Data.Array
import Data.Foldable
import Data.Monoid
import Data.Monoid.Endo
import qualified Data.Map as M

import KanColle.Expedition.Requirement
import KanColle.Expedition.Income
import KanColle.Expedition.Cost
import KanColle.Expedition.Base

validExpeditionId :: Int -> Boolean
validExpeditionId = getExpeditionRequirement >>> null >>> not

-- expeditionIds :: Array Int
-- expeditionIds = (1..32) <> (35..40)

getAvailableExpeditions :: forall a. Fleet a -> Array Int
getAvailableExpeditions fleet =
  filter (\eId -> checkExpedition eId fleet) allExpeditionIds

checkExpedition :: forall a. Int -> Fleet a -> Boolean
checkExpedition eId fleet = validExpeditionId eId
                         && null (unsatisfiedRequirements eId fleet)

type Expedition =
  { id :: Int
  , req :: ExpeditionRequirement
  , income :: Income
  , cost :: Cost
  }

allExpeditions :: Array Expedition
allExpeditions = map mkEntry allExpeditionIds
  where
    mkEntry eId = { id: eId
                  , req: getExpeditionRequirement eId
                  , income: getExpeditionIncome eId
                  , cost: getExpeditionCost eId }
