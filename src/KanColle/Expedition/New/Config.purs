module KanColle.Expedition.New.Config
  ({- Config
  , mkConfig
  , defConfig
  , defConfigs
  , getCompositionWithConfig
  , calcFleetActualCostTable
  , calcFleetNetIncomeTable

  , -} class IncomeModifier
  , getModifier
  ) where

import Prelude
import Data.Maybe
import KanColle.Expedition.Base
import KanColle.Expedition.New.Types
import KanColle.Expedition.New.MinCompo
import KanColle.Expedition.New.CostModel
import KanColle.Expedition.New.NetIncome
import KanColle.Expedition.New.Resource
import KanColle.Expedition.New.EArray
import KanColle.Expedition.New.Info
import KanColle.Expedition.New.Scorer
import Data.Array as A
import Data.List as L
import Data.Function
import Data.Traversable
import Data.Unfoldable hiding (fromMaybe)
import KanColle.Util
import Data.Int

class IncomeModifier im where
    getModifier :: im -> Number
    
class ResupplyCost rc where
    getResupplyCost :: rc -> Int {- ExpedId -} -> MaxCost
    
newtype NormalModifier = NormMod
  { greatSuccess :: Boolean
  , daihatsuCount :: Int
  }

instance normalIncomeModifier :: IncomeModifier NormalModifier where
   getModifier (NormMod nm) = gs' * (1.0 + 0.02 * toNumber nm.daihatsuCount)
     where
       gs' = if nm.greatSuccess then 1.5 else 1.0

newtype DirectModifier = DirectMod Number

instance directIncomeModifier :: IncomeModifier DirectModifier where
   getModifier (DirectMod v) = v
    
-- configuration for a single expedition.
data Config = Conf
  { greatSuccess :: Boolean
  , normDaihatsuCount :: Int -- normalized, should be one of [0..4]
  , wildcardSType :: SType
  }

{-
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

-- TODO: these table-makers might be separated as another module
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

calcResourcePerHrTable :: EArray Config -> CostModel -> Int -> Maybe (EArray ResourcePerHr)
calcResourcePerHrTable configs cm afkTimeInMin = do
    netIncomeTbl <- calcFleetNetIncomeTable configs cm
    let rphTbl = 
          imapEA (\eId ni ->
                   calcResourcePerHr ni (getInformation eId) afkTimeInMin) 
                 netIncomeTbl
    pure rphTbl
    
type ExpedId = Int
type EvalResult =
  { expedSet :: Array ExpedId
  , score :: Number
  }

evaluateExpeditions :: Scorer
                    -> EArray ResourcePerHr 
                    -> Array ExpedId
                    -> Int
                    -> Array EvalResult
evaluateExpeditions scorer rphTbl eCandidates fleetCount =
    A.sortBy (flip compare `on` (_.score)) (map evaluate expedSets)
  where
    expedSets :: Array (L.List ExpedId)
    expedSets = chooseN eCandidates fleetCount
    emptyRph = resourceRowsFill 0.0
    evaluate :: L.List ExpedId -> EvalResult
    evaluate expedSet =
        { expedSet: L.toUnfoldable expedSet
        , score: scorer rphSum (L.toUnfoldable infoList)
        }
      where
        infoList = map getInformation expedSet
        rphSum =
            foldl
              (resourceRowsLiftOp (+)) 
              emptyRph 
              (map (indEA rphTbl) expedSet)
-}
