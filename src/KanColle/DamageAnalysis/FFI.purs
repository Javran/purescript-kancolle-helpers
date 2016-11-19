module KanColle.DamageAnalysis.FFI
  ( analyzeBattleJS
  , analyzeNightBattleJS

  , analyzeSTFBattleJS
  , analyzeCTFBattleJS
  , analyzeTECFBattleJS
  , analyzeCombinedNightBattleJS

  , analyzeAbyssalCTFBattleJS
  , analyzeAbyssalCTFNightBattleJS
  
  , analyzeBothCombinedCTFBattleJS
  , analyzeBothCombinedSTFBattleJS
  , analyzeBothCombinedNightBattleJS
  ) where

import Prelude
import KanColle.Util
import KanColle.DamageAnalysis.DamageAnalysis
import KanColle.DamageAnalysis.Types
import KanColle.KCAPI.Battle
import Data.Maybe
import Data.Function.Uncurried
import Data.Nullable

readDameCon :: Array Int -> Array (Maybe DameCon)
readDameCon = map convert
  where
    convert :: Int -> Maybe DameCon
    convert 0 = Nothing
    convert 1 = Just RepairTeam
    convert 2 = Just RepairGoddess
    convert x = throwWith ("readDameCon: invalid input: " <> show x)

type FleetResult f = Array (f ShipResult)
type BattleResult f = NormalBattle (FleetResult f)
type BattleResultAC f = CombinedBattleAC (FleetResult f)
type GBattleResult f = GCombinedBattle (FleetResult f)
type CombinedBattleResult f = CombinedBattle (FleetResult f)

liftToFFI :: (Array (Maybe DameCon) -> Battle -> BattleResult Maybe)
          -> Fn2 (Array Int) Battle (BattleResult Nullable)
liftToFFI f = mkFn2 (\ds b -> convert (f (readDameCon ds) b))
  where
    convert s = { main: map toNullable s.main
                , enemy: map toNullable s.enemy }

liftToFFICombined :: (Array (Maybe DameCon) -> Battle -> CombinedBattleResult Maybe)
                  -> Fn2 (Array Int) Battle (CombinedBattleResult Nullable)
liftToFFICombined f = mkFn2 (\ds b -> convert (f (readDameCon ds) b))
  where
    convert s = { main: map toNullable s.main
                , enemy: map toNullable s.enemy
                , escort: map toNullable s.escort }

liftToFFIAC :: (Array (Maybe DameCon) -> Battle -> BattleResultAC Maybe)
                  -> Fn2 (Array Int) Battle (BattleResultAC Nullable)
liftToFFIAC f = mkFn2 (\ds b -> convert (f (readDameCon ds) b))
  where
    convert s = { main: map toNullable s.main
                , enemyMain: map toNullable s.enemyMain
                , enemyEscort: map toNullable s.enemyEscort }
                
liftToFFIBC :: (Array (Maybe DameCon) -> Battle -> GBattleResult Maybe)
                  -> Fn2 (Array Int) Battle (GBattleResult Nullable)
liftToFFIBC f = mkFn2 (\ds b -> convert (f (readDameCon ds) b))
  where
    convert s = { allyMain: map toNullable s.allyMain
                , allyEscort: map toNullable s.allyEscort
                , enemyMain: map toNullable s.enemyMain
                , enemyEscort: map toNullable s.enemyEscort }

analyzeBattleJS :: Fn2 (Array Int) Battle (BattleResult Nullable)
analyzeBattleJS = liftToFFI analyzeBattle

analyzeNightBattleJS :: Fn2 (Array Int) Battle (BattleResult Nullable)
analyzeNightBattleJS = liftToFFI analyzeNightBattle

analyzeSTFBattleJS :: Fn2 (Array Int) Battle (CombinedBattleResult Nullable)
analyzeSTFBattleJS = liftToFFICombined analyzeSTFBattle

analyzeCTFBattleJS :: Fn2 (Array Int) Battle (CombinedBattleResult Nullable)
analyzeCTFBattleJS = liftToFFICombined analyzeCTFBattle

analyzeTECFBattleJS :: Fn2 (Array Int) Battle (CombinedBattleResult Nullable)
analyzeTECFBattleJS = liftToFFICombined analyzeTECFBattle

analyzeCombinedNightBattleJS :: Fn2 (Array Int) Battle (BattleResult Nullable)
analyzeCombinedNightBattleJS = liftToFFI analyzeCombinedNightBattle

analyzeAbyssalCTFBattleJS :: Fn2 (Array Int) Battle (BattleResultAC Nullable)
analyzeAbyssalCTFBattleJS = liftToFFIAC analyzeAbyssalCTFBattle

analyzeAbyssalCTFNightBattleJS :: Fn2 (Array Int) Battle (BattleResultAC Nullable)
analyzeAbyssalCTFNightBattleJS = liftToFFIAC analyzeAbyssalCTFNightBattle

analyzeBothCombinedCTFBattleJS :: Fn2 (Array Int) Battle (GBattleResult Nullable)
analyzeBothCombinedCTFBattleJS = liftToFFIBC analyzeBothCombinedCTFBattle

analyzeBothCombinedSTFBattleJS :: Fn2 (Array Int) Battle (GBattleResult Nullable)
analyzeBothCombinedSTFBattleJS = liftToFFIBC analyzeBothCombinedSTFBattle

analyzeBothCombinedNightBattleJS :: Fn2 (Array Int) Battle (GBattleResult Nullable)
analyzeBothCombinedNightBattleJS = liftToFFIBC analyzeBothCombinedNightBattle
