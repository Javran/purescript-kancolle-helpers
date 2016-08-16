module KanColle.DamageAnalysis.DamageAnalysis
  ( analyzeBattle
  , analyzeNightBattle
  
  , analyzeSTFBattle
  , analyzeCTFBattle
  , analyzeTECFBattle
  , analyzeCombinedNightBattle
  ) where

import Prelude
import Data.Array
import Data.Maybe
import Data.Unfoldable
import KanColle.KCAPI.Battle
import KanColle.Util

import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Stages
import KanColle.DamageAnalysis.Types

rawSplit :: forall a. Array a -> LR (Array a)
rawSplit = fleetSplit true
    
normalSplit :: forall a. Array a -> LR (Array a)
normalSplit = fleetSplit false

-- in KCAPI there are cases where the length of the HP array
-- is neither 7 or 13, and it seems terminating "-1" elements
-- have a chance to disappear, so instead of using data from KCAPI
-- directly, we make sure the array is of the correct length
-- by applying this wrapper. "Nothing" will be appended to the
-- end of the list if the length is insufficient
ensureHpsLen :: Int -> Array (Maybe Int) -> Array (Maybe Int)
ensureHpsLen expectLen xs = case expectLen `compare` actualLen of
    LT -> throwWith "ensureHpsLen: array length is longer than expected"
    EQ -> xs
    GT -> (xs <> replicate (expectLen - actualLen) Nothing)
  where
    actualLen = length xs

-- get initial fleet info
getInitFleet :: Array (Maybe DameCon) -> Battle -> NormalFleetInfo Ship
getInitFleet ds battle = { main: allyShips, enemy: enemyShips }
  where
    nowHps = rawSplit (ensureHpsLen (1+12) $ getInitHps battle)
    allyNowHps = nowHps.left
    enemyNowHps = nowHps.right

    maxHps = rawSplit (ensureHpsLen (1+12) $ getMaxHps battle)
    allyMaxHps = maxHps.left
    enemyMaxHps = maxHps.right
    
    mkShip (Just hp) (Just fullHp) = Just { hp: hp, fullHp: fullHp, sunk: hp <= 0, dameCon: Nothing }
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
    
    escortNowHps = unsafeArrTail (ensureHpsLen (1+6) $ getInitHpsCombined battle)
    escortMaxHps = unsafeArrTail (ensureHpsLen (1+6) $ getMaxHpsCombined battle)
    
    mkShip (Just hp) (Just fullHp) = Just { hp: hp, fullHp: fullHp, sunk: hp <= 0, dameCon: Nothing }
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
    dupAsNormalBattle (zipWith getShipResult')
      `appNormalBattle` initFleet
      `appNormalBattle` finalFleet
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
    dupAsCombinedBattle (zipWith getShipResult')
      `appCombinedBattle` initFleet
      `appCombinedBattle` finalFleet
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
    dupAsNormalBattle (zipWith getShipResult')
      `appNormalBattle` initFleet
      `appNormalBattle` finalFleet
  where
    nightBattleInfo = getInitFleetCombined (replicate 6 Nothing <> ds) b
    initFleet = 
      { main: nightBattleInfo.escort
      , enemy: nightBattleInfo.enemy
      }
    finalFleet :: NormalFleetInfo Ship
    finalFleet = applyNormalDamageVector (nightBattleDV b) initFleet
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2
