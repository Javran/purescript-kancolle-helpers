module KanColle.Expedition.New.Config
  ( Config
  , mkConfig
  , defConfig
  , defConfigs
  , getCompositionWithConfig
  , calcFleetActualCostTable
  ) where

import Prelude
import Data.Maybe
import KanColle.Expedition.New.Types
import KanColle.Expedition.New.MinCompo
import KanColle.Expedition.New.CostModel
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
calcFleetActualCostTable configs cm = do
    expedMaxCosts <- unEA <$> mExpedMaxCosts
    let actualCosts = A.zipWith f expedMaxCosts allExpeds
        f maxCost eId = calcFleetActualCost maxCost (getInformation eId)
    pure (mkEA actualCosts)
  where
    allExpeds :: Array Int
    allExpeds = A.range 1 40
    concreteCompos :: EArray FleetCompo
    concreteCompos = mkEA (map (\eId -> getCompositionWithConfig (indEA configs eId) eId) allExpeds)
    mExpedMaxCosts :: Maybe (EArray FleetMaxCost)
    mExpedMaxCosts = 
        mkEA <$> 
          sequence 
            (unEA (map (\compo -> calcFleetMaxCost cm compo) concreteCompos))
