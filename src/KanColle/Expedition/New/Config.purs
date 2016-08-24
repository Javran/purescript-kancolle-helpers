module KanColle.Expedition.New.Config
  ( Config
  , mkConfig
  , defConfig
  , defConfigs
  , getCompositionWithConfig
  , calcFleetActualCostTable
  , calcFleetNetIncomeTable
  ) where

import Prelude
import Data.Maybe
import KanColle.Expedition.New.Types
import KanColle.Expedition.New.MinCompo
import KanColle.Expedition.New.CostModel
import KanColle.Expedition.New.NetIncome
import KanColle.Expedition.New.Resource
import KanColle.Expedition.New.EArray
import KanColle.Expedition.New.Info
import Data.Array as A
import Data.Unfoldable
import Data.Traversable

-- configuration for a single expedition.
data Config = Conf
  { greatSuccess :: Boolean
  , normDaihatsuCount :: Int -- normalized, should be one of [0..4]
  , wildcardSType :: SType
  }

mkConfig :: Boolean -> Int -> SType -> Config
mkConfig gs dCount wt = Conf
    { greatSuccess: gs
    , normDaihatsuCount: if dCount > 4 then 4 else dCount
    , wildcardSType: wt
    }

defConfig :: Config
defConfig = mkConfig false 0 DD

defConfigs :: EArray Config
defConfigs = mkEA (replicate 40 defConfig)

getCompositionWithConfig :: Config -> Int -> FleetCompo
getCompositionWithConfig (Conf c) n = if c.greatSuccess
    then compo <> replicate (6 - l) c.wildcardSType
    else compo
  where
    minCompo = getMinimumComposition n
    compo = map (fromMaybe c.wildcardSType) minCompo
    l = A.length compo

calcFleetActualCostTable :: EArray Config -> CostModel -> Maybe (EArray FleetActualCost)
calcFleetActualCostTable configs cm = imapEA f <$> mExpedMaxCosts
  where
    concreteCompos :: EArray FleetCompo
    concreteCompos = imapEA (flip getCompositionWithConfig) configs
    mExpedMaxCosts :: Maybe (EArray FleetMaxCost)
    mExpedMaxCosts =
        sequence (map (calcFleetMaxCost cm) concreteCompos)
    f eId maxCost = calcFleetActualCost maxCost (getInformation eId)

calcFleetNetIncomeTable :: EArray Config -> CostModel -> Maybe (EArray FleetNetIncome)
calcFleetNetIncomeTable configs cm = imapEA f <$> mACostTable
  where
    mACostTable :: Maybe (EArray FleetActualCost)
    mACostTable = calcFleetActualCostTable configs cm

    f :: Int -> FleetActualCost -> FleetNetIncome
    f eId = calcFleetNetIncome (getResource eId)
