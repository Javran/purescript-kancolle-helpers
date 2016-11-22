module KanColle.DamageAnalysis.DamageAnalysis
  ( analyzeBattle
  , analyzeNightBattle

  , analyzeSTFBattle
  , analyzeCTFBattle
  , analyzeTECFBattle
  , analyzeCombinedNightBattle

  , analyzeAbyssalCTFBattle
  , analyzeAbyssalCTFNightBattle

  , analyzeBothCombinedCTFBattle
  , analyzeBothCombinedSTFBattle
  , analyzeBothCombinedNightBattle
  ) where

import Prelude
import Data.Array
import Data.Tuple
import Data.Maybe
import Data.Unfoldable
import KanColle.KCAPI.Battle
import KanColle.Util

import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Stages
import KanColle.DamageAnalysis.Stages.CTF as CTF
import KanColle.DamageAnalysis.Stages.STF as STF
import KanColle.DamageAnalysis.Stages.BothCombinedCTF as BCTF
import KanColle.DamageAnalysis.Stages.BothCombinedSTF as BSTF
import KanColle.DamageAnalysis.Stages.AbyssalCombined as AC

import KanColle.DamageAnalysis.Types

normalSplit :: forall a. Array a -> LR (Array a)
normalSplit = fleetSplit false

-- in KCAPI there are cases where the length of the HP array
-- is neither 7 nor 13, and it seems terminating "-1" elements
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
    nowHps = normalSplit (ensureHpsLen 12 $ getInitHps battle)
    allyNowHps = nowHps.left
    enemyNowHps = nowHps.right

    maxHps = normalSplit (ensureHpsLen 12 $ getMaxHps battle)
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

    escortNowHps = ensureHpsLen 6 $ getInitHpsCombined battle
    escortMaxHps = ensureHpsLen 6 $ getMaxHpsCombined battle

    mkShip (Just hp) (Just fullHp) = Just { hp: hp, fullHp: fullHp, sunk: hp <= 0, dameCon: Nothing }
    mkShip _ _ = Nothing

    addDameCon (Just ship) dc = Just (ship { dameCon = dc })
    addDameCon _ _ = Nothing

    escortShips :: FleetInfo Ship
    escortShips = zipWith addDameCon (zipWith mkShip escortNowHps escortMaxHps) dsEscort

getInitFleetAC :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfoAC Ship
getInitFleetAC ds battle =
    { main: allyShips
    , enemyMain: enemyMainShips
    , enemyEscort: enemyEscortShips
    }
  where
    -- ally vs. enemy main
    allyEMainNowHps = normalSplit (ensureHpsLen 12 $ getInitHps battle)
    allyNowHps = allyEMainNowHps.left
    enemyMainNowHps = allyEMainNowHps.right

    allyEMainMaxHps = normalSplit (ensureHpsLen 12 $ getMaxHps battle)
    allyMaxHps = allyEMainMaxHps.left
    enemyMainMaxHps = allyEMainMaxHps.right

    -- <null> vs. enemy escort
    nullEEscortNowHps = normalSplit (ensureHpsLen 12 $ getInitHpsCombined battle)
    enemyEscortNowHps = nullEEscortNowHps.right

    nullEEscortMaxHps = normalSplit (ensureHpsLen 12 $ getMaxHpsCombined battle)
    enemyEscortMaxHps = nullEEscortMaxHps.right

    mkShip (Just hp) (Just fullHp) = Just
        { hp: hp, fullHp: fullHp
        , sunk: hp <= 0, dameCon: Nothing }
    mkShip _ _ = Nothing

    addDameCon (Just ship) dc = Just (ship { dameCon = dc })
    addDameCon _ _ = Nothing

    allyShips :: FleetInfo Ship
    allyShips = zipWith addDameCon (zipWith mkShip allyNowHps allyMaxHps) ds

    enemyMainShips :: FleetInfo Ship
    enemyMainShips = zipWith mkShip enemyMainNowHps enemyMainMaxHps

    enemyEscortShips :: FleetInfo Ship
    enemyEscortShips = zipWith mkShip enemyEscortNowHps enemyEscortMaxHps

getInitFleetBC :: Array (Maybe DameCon) -> Battle -> GCombinedFleetInfo Ship
getInitFleetBC ds battle =
    { allyMain: zipWith addDameCon (zipWith mkShip allyMainNowHps allyMainMaxHps) dsPair.left
    , allyEscort: zipWith addDameCon (zipWith mkShip allyEscortNowHps allyEscortMaxHps) dsPair.right
    , enemyMain: zipWith mkShip enemyMainNowHps enemyMainMaxHps
    , enemyEscort: zipWith mkShip enemyEscortNowHps enemyEscortMaxHps
    }
  where
    allyEMainNowHps = normalSplit (ensureHpsLen 12 $ getInitHps battle)
    allyMainNowHps = allyEMainNowHps.left
    enemyMainNowHps = allyEMainNowHps.right

    allyEMainMaxHps = normalSplit (ensureHpsLen 12 $ getMaxHps battle)
    allyMainMaxHps = allyEMainMaxHps.left
    enemyMainMaxHps = allyEMainMaxHps.right

    allyEEscortNowHps = normalSplit (ensureHpsLen 12 $ getInitHpsCombined battle)
    allyEscortNowHps = allyEEscortNowHps.left
    enemyEscortNowHps = allyEEscortNowHps.right

    allyEEscortMaxHps = normalSplit (ensureHpsLen 12 $ getMaxHpsCombined battle)
    allyEscortMaxHps = allyEEscortMaxHps.left
    enemyEscortMaxHps = allyEEscortMaxHps.right

    mkShip (Just hp) (Just fullHp) = Just
        { hp: hp, fullHp: fullHp
        , sunk: hp <= 0, dameCon: Nothing }
    mkShip _ _ = Nothing

    addDameCon (Just ship) dc = Just (ship { dameCon = dc })
    addDameCon _ _ = Nothing

    dsPair = normalSplit ds

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
analyzeSTFBattle = analyzeCombinedBattleBy STF.battleDV

analyzeCTFBattle :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfo ShipResult
analyzeCTFBattle = analyzeCombinedBattleBy CTF.battleDV

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

analyzeAbyssalCTFBattle :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfoAC ShipResult
analyzeAbyssalCTFBattle ds b =
    { main: z (_.main)
    , enemyMain: z (_.enemyMain)
    , enemyEscort: z (_.enemyEscort)
    }
  where
    z prj = zipWith getShipResult' (prj initFleet) (prj finalFleet)
    initFleet = getInitFleetAC ds b
    resultDV = AC.battleDV b
    finalFleet =
      { main: applyDamageVector resultDV.main initFleet.main
      , enemyMain: applyDamageVector resultDV.enemyMain initFleet.enemyMain
      , enemyEscort: applyDamageVector resultDV.enemyEscort initFleet.enemyEscort
      }
    getShipResult' :: Maybe Ship -> Maybe Ship -> Maybe ShipResult
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2

analyzeAbyssalCTFNightBattle :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfoAC ShipResult
analyzeAbyssalCTFNightBattle ds b =
    { main: z (_.main)
    , enemyMain: z (_.enemyMain)
    , enemyEscort: z (_.enemyEscort)
    }
  where
    z prj = zipWith getShipResult' (prj allInitFleet) (prj allFinalFleet)
    getShipResult' :: Maybe Ship -> Maybe Ship -> Maybe ShipResult
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2

    allInitFleet = getInitFleetAC ds b
    enemySplitted = case getEnemyActiveDeck b of
        1 -> Tuple allInitFleet.enemyMain allInitFleet.enemyEscort
        2 -> Tuple allInitFleet.enemyEscort allInitFleet.enemyMain
        _ -> throwWith "invalid api_active_deck value"
    resultDV = nightBattleDV b
    initFleet = { main: allInitFleet.main, enemy: fst enemySplitted }
    finalFleet = applyNormalDamageVector resultDV initFleet
    allFinalFleet = case getEnemyActiveDeck b of
        1 ->
          { main: finalFleet.main
          , enemyMain: finalFleet.enemy
          , enemyEscort: snd enemySplitted }
        2 ->
          { main: finalFleet.main
          , enemyMain: snd enemySplitted
          , enemyEscort: finalFleet.enemy }
        _ -> throwWith "invalid api_active_deck value"

analyzeBothCombinedCTFBattle :: Array (Maybe DameCon) -> Battle -> GCombinedFleetInfo ShipResult
analyzeBothCombinedCTFBattle ds b =
    { allyMain: z (_.allyMain)
    , allyEscort: z (_.allyEscort)
    , enemyMain: z (_.enemyMain)
    , enemyEscort: z (_.enemyEscort)
    }
  where
    initFleet = getInitFleetBC ds b
    resultDV = BCTF.battleDV b
    finalFleet =
      { allyMain: applyDamageVector resultDV.allyMain initFleet.allyMain
      , allyEscort: applyDamageVector resultDV.allyEscort initFleet.allyEscort
      , enemyMain: applyDamageVector resultDV.enemyMain initFleet.enemyMain
      , enemyEscort: applyDamageVector resultDV.enemyEscort initFleet.enemyEscort
      }
    getShipResult' :: Maybe Ship -> Maybe Ship -> Maybe ShipResult
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2
    z prj = zipWith getShipResult' (prj initFleet) (prj finalFleet)

analyzeBothCombinedSTFBattle :: Array (Maybe DameCon) -> Battle -> GCombinedFleetInfo ShipResult
analyzeBothCombinedSTFBattle ds b =
    { allyMain: z (_.allyMain)
    , allyEscort: z (_.allyEscort)
    , enemyMain: z (_.enemyMain)
    , enemyEscort: z (_.enemyEscort)
    }
  where
    initFleet = getInitFleetBC ds b
    resultDV = BSTF.battleDV b
    finalFleet =
      { allyMain: applyDamageVector resultDV.allyMain initFleet.allyMain
      , allyEscort: applyDamageVector resultDV.allyEscort initFleet.allyEscort
      , enemyMain: applyDamageVector resultDV.enemyMain initFleet.enemyMain
      , enemyEscort: applyDamageVector resultDV.enemyEscort initFleet.enemyEscort
      }
    getShipResult' :: Maybe Ship -> Maybe Ship -> Maybe ShipResult
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2
    z prj = zipWith getShipResult' (prj initFleet) (prj finalFleet)

analyzeBothCombinedNightBattle :: Array (Maybe DameCon) -> Battle
                                  -> GCombinedFleetInfo ShipResult
analyzeBothCombinedNightBattle ds b =
    { allyMain: z (_.allyMain)
    , allyEscort: z (_.allyEscort)
    , enemyMain: z (_.enemyMain)
    , enemyEscort: z (_.enemyEscort)
    }
  where
    z prj = zipWith getShipResult' (prj allInitFleet) (prj allFinalFleet)
    getShipResult' :: Maybe Ship -> Maybe Ship -> Maybe ShipResult
    getShipResult' ms1 ms2 = getShipResult <$> ms1 <*> ms2

    allInitFleet = getInitFleetBC ds b
    enemySplitted = case getEnemyActiveDeck b of
        1 -> Tuple allInitFleet.enemyMain allInitFleet.enemyEscort
        2 -> Tuple allInitFleet.enemyEscort allInitFleet.enemyMain
        _ -> throwWith "invalid api_active_deck value"
    resultDV = nightBattleDV b
    initFleet = { main: allInitFleet.allyEscort, enemy: fst enemySplitted }
    finalFleet = applyNormalDamageVector resultDV initFleet
    allFinalFleet = case getEnemyActiveDeck b of
        1 ->
          { allyMain: allInitFleet.allyMain
          , allyEscort: finalFleet.main
          , enemyMain: finalFleet.enemy
          , enemyEscort: snd enemySplitted }
        2 ->
          { allyMain: allInitFleet.allyMain
          , allyEscort: finalFleet.main
          , enemyMain: snd enemySplitted
          , enemyEscort: finalFleet.enemy }
        _ -> throwWith "invalid api_active_deck value"
