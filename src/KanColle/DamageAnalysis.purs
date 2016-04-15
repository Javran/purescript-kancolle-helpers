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

type ShipResult =
  { hp :: Int
  , sunk :: Boolean
  , dameConConsumed :: Boolean
  }

-- invariant: the length is always 6
type FleetInfo a = Array (Maybe a)

type NormalFleetInfo a = NormalBattle (FleetInfo a)
  
type CombinedFleetInfo a = CombinedBattle (FleetInfo a)
  
applyDamageVector :: DamageVector -> FleetInfo Ship -> FleetInfo Ship
applyDamageVector (DV2 dv) fleet = zipWith combine dv fleet
  where
    combine :: Damage -> Maybe Ship -> Maybe Ship
    combine dmg ms = applyDamage dmg <$> ms

applyNormalDamageVector :: NormalDamageVector -> NormalFleetInfo Ship -> NormalFleetInfo Ship
applyNormalDamageVector ndv fleet =
    dupAsNormalBattle applyDamageVector
      `appNormalBattle` ndv 
      `appNormalBattle` fleet
      
applyCombinedDamageVector :: CombinedDamageVector -> CombinedFleetInfo Ship -> CombinedFleetInfo Ship
applyCombinedDamageVector ndv fleet =
    dupAsCombinedBattle applyDamageVector
      `appCombinedBattle` ndv 
      `appCombinedBattle` fleet

getShipResult :: Ship -> Ship -> ShipResult
getShipResult sBefore sAfter =
  { hp: sAfter.hp
  , sunk: sAfter.sunk
  , dameConConsumed: 
      isJust sBefore.dameCon && isNothing sAfter.dameCon
  }
  
rawSplit :: forall a. Array a -> {left :: Array a, right :: Array a}
rawSplit raws = if check
    then { left: slice 1 7 raws
         , right: slice 7 13 raws }
    else throwWith "rawSplit: input array length should be 13"
  where
    check = length raws == 13
    
normalSplit :: forall a. Array a -> {left :: Array a, right :: Array a}
normalSplit raws = if check
    then { left: slice 0 6 raws
         , right: slice 6 12 raws }
    else throwWith "normalSplit: input array length should be 12"
  where
    check = length raws == 12

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
analyzeBattleBy getDV ds battle =
    { main: zipWith getShipResult' initFleet.main finalFleet.main
    , enemy: zipWith getShipResult' initFleet.enemy finalFleet.enemy
    }
  where
    initFleet = getInitFleet ds battle
    finalFleet :: NormalFleetInfo Ship
    finalFleet = applyNormalDamageVector (getDV battle) initFleet
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2

analyzeCombinedBattleBy :: (Battle -> CombinedDamageVector)
                -> Array (Maybe DameCon) 
                -> Battle
                -> CombinedFleetInfo ShipResult
analyzeCombinedBattleBy getDV ds battle =
    { main: zipWith getShipResult' initFleet.main finalFleet.main
    , enemy: zipWith getShipResult' initFleet.enemy finalFleet.enemy
    , escort: zipWith getShipResult' initFleet.escort finalFleet.escort
    }
  where
    initFleet = getInitFleetCombined ds battle
    finalFleet = applyCombinedDamageVector (getDV battle) initFleet
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
