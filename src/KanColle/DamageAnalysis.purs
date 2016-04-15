module KanColle.DamageAnalysis where

import Prelude
import Data.Array
import Data.Array.Unsafe as AU
import Data.Maybe
import KanColle.KCAPI.Battle
import KanColle.Util

import KanColle.DamageAnalysis.Damage
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Stages
import KanColle.DamageAnalysis.Types

rawSplit :: forall a. Array a -> {left :: Array a, right :: Array a}
rawSplit = fleetSplit true
    
normalSplit :: forall a. Array a -> {left :: Array a, right :: Array a}
normalSplit = fleetSplit false

-- get initial fleet info
getInitFleet :: Array (Maybe DameCon) -> Battle -> NormalFleetInfo Ship
getInitFleet ds battle = { main: allyShips, enemy: enemyShips }
  where
    nowHps = rawSplit (getInitHps battle)
    allyNowHps = nowHps.left
    enemyNowHps = nowHps.right

    maxHps = rawSplit (getMaxHps battle)
    allyMaxHps = maxHps.left
    enemyMaxHps = maxHps.right
    
    mkShip (Just hp) (Just fullHp) = Just { hp: hp, fullHp: fullHp, sunk: false, dameCon: Nothing }
    mkShip _ _ = Nothing
    
    addDameCon (Just ship) dc = Just (ship { dameCon = dc })
    addDameCon _ _ = Nothing
  
    allyShips :: FleetInfo Ship
    allyShips = zipWith addDameCon (zipWith mkShip allyNowHps allyMaxHps) ds

    enemyShips :: FleetInfo Ship
    enemyShips = zipWith mkShip enemyNowHps enemyMaxHps
    
getInitFleetCombined :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfo Ship
getInitFleetCombined ds battle =
    { main: initFleet.main
    , escort: escortShips
    , enemy: initFleet.enemy
    }
  where
    dsSplitted = normalSplit ds
    dsMain = dsSplitted.left
    dsEscort = dsSplitted.right
    
    initFleet = getInitFleet dsMain battle
    
    escortNowHps = AU.tail (getInitHpsCombined battle)
    escortMaxHps = AU.tail (getMaxHpsCombined battle)
    
    mkShip (Just hp) (Just fullHp) = Just { hp: hp, fullHp: fullHp, sunk: false, dameCon: Nothing }
    mkShip _ _ = Nothing
    
    addDameCon (Just ship) dc = Just (ship { dameCon = dc })
    addDameCon _ _ = Nothing
  
    escortShips :: FleetInfo Ship
    escortShips = zipWith addDameCon (zipWith mkShip escortNowHps escortMaxHps) dsEscort

analyzeBattle :: Array (Maybe DameCon) -> Battle -> NormalFleetInfo ShipResult
analyzeBattle = analyzeBattleBy battleDV

analyzeNightBattle :: Array (Maybe DameCon) -> Battle -> NormalFleetInfo ShipResult
analyzeNightBattle = analyzeBattleBy nightBattleDV

analyzeBattleBy :: (Battle -> NormalDamageVector)
                -> Array (Maybe DameCon) 
                -> Battle
                -> NormalFleetInfo ShipResult
analyzeBattleBy getDVFromBattle ds battle =
    { main: zipWith getShipResult' initFleet.main finalFleet.main
    , enemy: zipWith getShipResult' initFleet.enemy finalFleet.enemy
    }
  where
    initFleet = getInitFleet ds battle
    finalFleet :: NormalFleetInfo Ship
    finalFleet = applyNormalDamageVector (getDVFromBattle battle) initFleet
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2

analyzeCombinedBattleBy :: (Battle -> CombinedDamageVector)
                -> Array (Maybe DameCon) 
                -> Battle
                -> CombinedFleetInfo ShipResult
analyzeCombinedBattleBy getDVFromBattle ds battle =
    { main: zipWith getShipResult' initFleet.main finalFleet.main
    , enemy: zipWith getShipResult' initFleet.enemy finalFleet.enemy
    , escort: zipWith getShipResult' initFleet.escort finalFleet.escort
    }
  where
    initFleet = getInitFleetCombined ds battle
    finalFleet = applyCombinedDamageVector (getDVFromBattle battle) initFleet
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2

analyzeSTFBattle :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfo ShipResult
analyzeSTFBattle = analyzeCombinedBattleBy battleSurfaceTaskForceDV

analyzeCTFBattle :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfo ShipResult
analyzeCTFBattle = analyzeCombinedBattleBy battleCarrierTaskForceDV

analyzeTECFBattle :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfo ShipResult
analyzeTECFBattle = analyzeCTFBattle

analyzeCombinedNightBattle :: Array (Maybe DameCon) -> Battle -> NormalFleetInfo ShipResult
analyzeCombinedNightBattle ds b =
    { main: zipWith getShipResult' initFleet.main finalFleet.main
    , enemy: zipWith getShipResult' initFleet.enemy finalFleet.enemy
    }
  where
    nightBattleInfo = getInitFleetCombined (replicate 6 Nothing <> ds) b
    initFleet = 
      { main: nightBattleInfo.escort
      , enemy: nightBattleInfo.enemy
      }
    finalFleet :: NormalFleetInfo Ship
    finalFleet = applyNormalDamageVector (nightBattleDV b) initFleet
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2
