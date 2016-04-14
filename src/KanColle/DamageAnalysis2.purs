module KanColle.DamageAnalysis2 where

import Prelude
import Data.Array
import Data.Array.Unsafe as AU
import Data.Maybe
import KanColle.KCAPI.Battle
import KanColle.Util

import KanColle.DamageAnalysis.Damage
import KanColle.DamageAnalysis.DamageVector2
import KanColle.DamageAnalysis.Stages2

type ShipResult =
  { hp :: Int
  , sunk :: Boolean
  , dameConConsumed :: Boolean
  }

-- invariant: the length is always 6
type FleetInfo a = Array (Maybe a)

type NormalFleetInfo a =
  { main :: FleetInfo a
  , enemy :: FleetInfo a
  }
  
type CombinedFleetInfo a =
  { main :: FleetInfo a
  , escort :: FleetInfo a
  , enemy :: FleetInfo a
  }
  
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

analyzeBattleBy :: (Battle -> DamageVector2)
                -> Array (Maybe DameCon) 
                -> Battle
                -> NormalFleetInfo ShipResult
analyzeBattleBy getDV ds battle =
    { main: zipWith getShipResult' initFleet.main finalFleet.main
    , enemy: zipWith getShipResult' initFleet.enemy finalFleet.enemy
    }
  where
    initFleet = getInitFleet ds battle
    finalFleet = applyDamageVector (getDV battle) initFleet
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2

applyDamageVector :: DamageVector2
                  -> NormalFleetInfo Ship
                  -> NormalFleetInfo Ship
applyDamageVector (DV2 dv) xs =
    { main: applyDV xs.main allyDv
    , enemy: applyDV xs.enemy enemyDv
    }
  where
    splitted = normalSplit dv
    allyDv = splitted.left
    enemyDv = splitted.right

    applyDV fleet dv = zipWith combine fleet dv
      where
        combine :: Maybe Ship -> Damage -> Maybe Ship
        combine ms dmg = applyDamage dmg <$> ms
        
applyCombinedDamageVector :: CombinedDamageVector2
                          -> CombinedFleetInfo Ship
                          -> CombinedFleetInfo Ship
applyCombinedDamageVector (CDV cdv) xs =
    { main: phaseResult1Main
    , escort: phaseResult2Escort
    , enemy: phaseResult3Enemy
    }
  where
    -- phase 1: main fleet & enemy
    fleetInfo1 = {main: xs.main, enemy: xs.enemy}
    phaseResult1 = applyDamageVector cdv.main fleetInfo1
    phaseResult1Main = phaseResult1.main
    phaseResult1Enemy = phaseResult1.enemy
    
    -- phase 2: escort fleet & enemy
    fleetInfo2 = {main: xs.escort, enemy: phaseResult1Enemy}
    phaseResult2 = applyDamageVector cdv.escort fleetInfo2
    phaseResult2Escort = phaseResult2.main
    phaseResult2Enemy = phaseResult2.enemy
    
    -- phase 3: support fleet & enemy
    fleetInfo3 = {main: replicate 6 Nothing, enemy: phaseResult2Enemy}
    phaseResult3 = applyDamageVector cdv.support fleetInfo3
    phaseResult3Enemy = phaseResult3.enemy
    
analyzeCombinedBattleBy :: (Battle -> CombinedDamageVector2)
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
    finalFleet = applyDamageVector (nightBattleDV b) initFleet
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2
