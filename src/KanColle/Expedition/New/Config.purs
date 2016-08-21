module KanColle.Expedition.New.Config
  ( Config
  , mkConfig
  , defConfig
  , getCompositionWithConfig
  ) where

import Prelude
import Data.Maybe
import KanColle.Expedition.New.Types
import KanColle.Expedition.New.MinCompo
import KanColle.Expedition.New.CostModel
import KanColle.Expedition.New.EArray
import Data.Array as A
import Data.Unfoldable

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

getCompositionWithConfig :: Config -> Int -> FleetCompo
getCompositionWithConfig (Conf c) n = if c.greatSuccess
    then compo <> replicate (6 - l) c.wildcardSType
    else compo
  where
    minCompo = getMinimumComposition n
    compo = map (fromMaybe c.wildcardSType) minCompo
    l = A.length compo

calcFleetActualCostTable :: EArray Config -> Maybe (EArray FleetActualCost)
calcFleetActualCostTable configs = Nothing -- TODO
  where
    allExpeds :: Array Int
    allExpeds = A.range 1 40
    concreteCompos :: EArray FleetCompo
    concreteCompos = mkEA (map (\eId -> getCompositionWithConfig (indEA configs eId) eId) allExpeds)
