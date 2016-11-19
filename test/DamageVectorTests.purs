module DamageVectorTests where

import Prelude
import Test.Unit
import Data.Foreign

import Data.String as Str
import Data.Array
import Data.Monoid
import Data.Unfoldable
import Data.Maybe
import Data.Function
import KanColle.Util
import KanColle.DamageAnalysis
import KanColle.DamageAnalysis.Stages
import KanColle.DamageAnalysis.FFI
import Test.Unit.Assert as Assert
import Data.Foldable
import Partial.Unsafe

import KanColle.KCAPI.Battle

import BattleData

dc6 :: Array (Maybe DameCon)
dc6 = replicate 6 Nothing

dc12 :: Array (Maybe DameCon)
dc12 = replicate 12 Nothing

dvToStr :: DamageVector -> String
dvToStr dv = Str.joinWith "," $ map (damageToInt >>> show) (getDV dv)

ndvToStr :: LR DamageVector -> String
ndvToStr ndv = dvToStr ndv.left <> " -- " <> dvToStr ndv.right

acDVtoStr :: LR (LR DamageVector) -> String
acDVtoStr dvdv = dvToStr dvdv.left.left <> " -- " <> dvToStr dvdv.right.left <> " && " <> dvToStr dvdv.right.right

bothDVtoStr :: LR (LR DamageVector) -> String
bothDVtoStr dvdv = 
    dvToStr dvdv.left.left <> " && " <> dvToStr dvdv.left.right <> " -- " 
 <> dvToStr dvdv.right.left <> " && " <> dvToStr dvdv.right.right

testDameCon :: forall e. TestSuite e
testDameCon = do
   test "Damage: without damecon" do
      Assert.equal (1-20) (applyDamage (mkDamage 20) (mkShip 1 21 Nothing)).hp
   test "Damage: with repair team" do
      let ship = (applyDamage (mkDamage 20) (mkShip 1 21 $ Just RepairTeam))
      Assert.equal 4 ship.hp
      Assert.equal Nothing ship.dameCon
   test "Damage: with repair goddess" do
      Assert.equal 21 (applyDamage (mkDamage 20) (mkShip 1 21 $ Just RepairGoddess)).hp
   test "Damage: chained damage" do
      -- 21/21 -> -20 -> 1/21 -> -30 -> -29/21 (sinking) -> damecon consumed 21/21 -> -20 -> 1/21
      Assert.equal 1 (applyDamage (foldMap mkDamage [20,30,20]) (mkShip 21 21 $ Just RepairGoddess)).hp

testDamageVector :: forall e. TestSuite e
testDamageVector = do
    test "DamageVector: first aerial battle stage" do
      Assert.assert "sample battle1" $
          "0,0,0,4,0,0 -- 0,0,21,0,164,0" == ndvToStr (koukuDV battle1)
      Assert.assert "sample battle2" $
          ndvToStr memptyLR == ndvToStr (koukuDV battle2)
      Assert.assert "sample aerialBattle1" $
          ndvToStr memptyLR == ndvToStr (koukuDV aerialBattle1)
      Assert.assert "sample withSupportExpedition1" $
          "15,0,0,7,8,0 -- 55,0,0,0,0,0"
              == ndvToStr (koukuDV withSupportExpedition1)
    test "DamageVector: support fleet attack stage" do
      let dvF b = supportAirAttackDV b <> supportHouraiDV b
      Assert.assert "sample battle1" $
          dvToStr mempty == dvToStr (dvF battle1)
      Assert.assert "sample withSupportExpedition1" $
          "0,29,0,0,101,0"
              == dvToStr (dvF withSupportExpedition1)
      Assert.assert "sample withSupportExpedition2" $
          "0,70,20,132,40,0"
              == dvToStr (dvF withSupportExpedition2)
    test "DamageVector: abyssal combined: LBAS" do
      let dv :: LR (LR DamageVector)
          dv = landBasedAirStrikeDVsAC abyssalCombinedFleet1
      Assert.assert "sample1 (LBAS)" $
          "0,0,0,0,0,0 -- 0,0,0,0,0,0 && 0,0,0,122,0,176"
              == acDVtoStr dv
    test "DamageVector: abyssal combined: kouku" do
      let dv :: LR (LR DamageVector)
          dv = koukuDVAC abyssalCombinedFleet1
      Assert.assert "sample1 (kouku)" $
          "0,0,0,0,0,0 -- 63,0,10,0,0,0 && 0,0,0,0,0,0"
              == acDVtoStr dv
    test "DamageVector: abyssal combined: opening raigeki DV" do
      let raw :: Raigeki
          raw = unsafePartial (fromJust (getOpeningAttack abyssalCombinedFleet1))
          dv :: LR (LR DamageVector)
          dv = calcRaigekiDamageAC raw
      Assert.assert "sample1 (openning raigeki)" $
          "0,0,0,0,0,0 -- 0,0,0,0,0,73 && 0,0,0,0,0,0"
              == acDVtoStr dv
    test "DamageVector: abyssal combined: regular raigeki DV" do
      let raw :: Raigeki
          raw = unsafePartial (fromJust (getRaigekiAC abyssalCombinedFleet1))
          dv :: LR (LR DamageVector)
          dv = calcRaigekiDamageAC raw
      Assert.assert "sample1" $
          "0,0,4,0,0,0 -- 0,0,0,0,0,0 && 0,0,4,0,0,0"
              == acDVtoStr dv
    test "DamageVector: abyssal combined: hougeki1" do
      let raw :: Hougeki
          raw = unsafePartial (fromJust (getHougeki1AC abyssalCombinedFleet1))
          dv :: LR (LR DamageVector)
          dv = calcHougekiDamageAC raw
      Assert.assert "sample1" $
          "9,0,0,0,0,0 -- 0,0,0,0,0,0 && 232,5,5,0,101,0"
              == acDVtoStr dv
    test "DamageVector: abyssal combined: hougeki2" do
      let raw :: Hougeki
          raw = unsafePartial (fromJust (getHougeki2AC abyssalCombinedFleet1))
          dv :: LR (LR DamageVector)
          dv = calcHougekiDamageAC raw
      Assert.assert "sample1" $
          "0,0,0,0,0,0 -- 37,71,62,178,0,0 && 0,0,0,0,0,0"
              == acDVtoStr dv
    test "DamageVector: abyssal combined: hougeki3" do
      let raw :: Hougeki
          raw = unsafePartial (fromJust (getHougeki3AC abyssalCombinedFleet1))
          dv :: LR (LR DamageVector)
          dv = calcHougekiDamageAC raw
      Assert.assert "sample1" $
          "0,0,0,0,0,0 -- 25,148,161,0,93,0 && 0,0,58,0,0,0"
              == acDVtoStr dv
    test "DamageVector: bothCombinedCTF: hougeki1" do
      let dv :: LR (LR DamageVector)
          dv = hougeki1BCDV bothCombinedCTF1
      Assert.assert "sample1" $
          "0,25,0,0,0,0 && 0,0,0,0,0,0 -- 0,154,0,79,0,133 && 0,0,0,0,0,0" == bothDVtoStr dv
    test "DamageVector: bothCombinedCTF: hougeki2" do
      let dv :: LR (LR DamageVector)
          dv = hougeki2BCDV bothCombinedCTF1
      Assert.assert "sample1" $
          "0,0,0,0,0,0 && 0,0,0,6,7,0 -- 0,0,0,0,0,0 && 261,116,7,55,234,71" == bothDVtoStr dv
    test "DamageVector: bothCombinedCTF: raigeki" do
      let dv :: LR (LR DamageVector)
          dv = raigekiBCDV bothCombinedCTF1
      Assert.assert "sample1" $
          "0,0,0,0,0,0 && 0,0,0,0,0,0 -- 34,0,0,0,0,0 && 0,0,237,0,0,0" == bothDVtoStr dv
    test "DamageVector: bothCombinedCTF: hougeki3" do
      let dv :: LR (LR DamageVector)
          dv = hougeki3BCDV bothCombinedCTF1
      Assert.assert "sample1" $
          "0,0,0,0,0,0 && 0,0,0,0,13,0 -- 167,0,0,98,0,0 && 0,0,0,0,0,0" == bothDVtoStr dv
    test "DamageVector: bothCombinedCTF: opening" do
      let dv :: LR (LR DamageVector)
          dv = openingDVAC bothCombinedCTF1
      Assert.assert "sample1" $
          "0,0,0,0,0,0 && 0,0,0,0,0,0 -- 0,0,91,0,0,0 && 0,0,0,0,0,0" == bothDVtoStr dv
    test "DamageVector: bothCombinedCTF: kouku" do
      let dv :: LR (LR DamageVector)
          dv = koukuDVBC bothCombinedCTF1
      Assert.assert "sample1" $
          "0,0,0,0,0,0 && 0,4,0,0,0,0 -- 0,0,0,0,65,0 && 0,0,0,0,0,0" == bothDVtoStr dv

testDamageAnalyzer :: forall e. TestSuite e
testDamageAnalyzer =
    test "DamageAnalyzer" do
      Assert.assert "battle sample 1" $
        (merge >>> trimInfo) (analyzeBattle dc6 battle1) ==
          map Just [75,75,75,75-4,77,79-9
                   ,84-127,60-95,60-21-98,53-213-145,35-164,35-159-157]
      Assert.assert "battle sample 2" $
        (merge >>> trimInfo) (analyzeBattle dc6 battle2) ==
          map toMaybeInt [15,15-3,18,18,9999,9999
                         ,90-81-41,55-116,28,28-236,70-121,70-131]
      Assert.assert "night battle sample 1" $
        (merge >>> trimInfo) (analyzeNightBattle dc6 nightBattle1) ==
          map toMaybeInt [15,12,18,18,9999,9999,0,0,28-94,0,0,0]
      Assert.assert "night battle sample 2" $
        (merge >>> trimInfo) (analyzeNightBattle dc6 nightBattle2) ==
          map toMaybeInt [75,75,6,69,23,43,0,41-181,21-124,0,0,9999]
      Assert.assert "aerial battle sample 1" $
        (merge >>> trimInfo) (analyzeBattle dc6 aerialBattle1) ==
          map toMaybeInt [37,31,31,32,32,44-6,96,70,60,20,20,20]
      Assert.assert "ld aerial battle sample 1" $
        (merge >>> trimInfo) (analyzeBattle dc6 ldAerialBattle1) ==
          map toMaybeInt [31,56,56,42,33,29,500,9999,9999,9999,9999,9999]
      Assert.assert "night battle repair team sample 1" $
        (merge >>> trimInfo) (analyzeNightBattle repairTeam nightBattleWithDameCon1) ==
          map toMaybeInt [90,25, 13{- sinking: 22-22 = 0 -},35-27-6,26,12, 133,0,146-45-64,0,0,34]
      Assert.assert "battle with repair team sample 1" $
        (merge >>> trimInfo) (analyzeBattle repairTeam normBattleWithDameCon1) ==
          map toMaybeInt [36-28,72-5-35,50-28-11-4,8,{- sinking 5-103 -}7-3,59-34,
                          480-47-37,130,130-22-7-10-128,130-10-16-44,600-33-47,130-10]
      Assert.assert "battle with land-based airstrike" $
        (merge >>> trimInfo) (analyzeBattle dc6 normBattleWithLandBasedAir1) ==
          map toMaybeInt [63-5,52,68,29-2-19,31-20,11,
                          640-166-10-17-99-34-24-10-15-13-22,
                          70-16-148,98-10-12-210,98-12-19-9-9,35-59,35-93]
      Assert.assert "battle with land-based airstrike (combined)" $
        (mergeCombined >>> trimInfo) (analyzeSTFBattle dc12 combinedFleetWithLandBasedAir1) ==
          map toMaybeInt [81,82,58,50,57,56,
                          45,53,55,31,43,31,
                          57-42-247,35-98,35-52,20-45,20-66,20-125]
      Assert.assert "battle with land-based airstrike (2)" $
        (merge >>> trimInfo) (analyzeBattle dc6 normBattleWithLandBasedAir2) ==
          map toMaybeInt [57,30,27-5,12,37,45,
                          600,500,500, 9999,9999,9999]
      Assert.assert "battle with opening taisen" $
        (merge >>> trimInfo) (analyzeBattle dc6 normBattleWithOpeningTaisen1) ==
          map toMaybeInt [39,77,44-4,47, 9999,9999,
                          44-2-42,27-56,27-36,19-76, 9999,9999]
      Assert.assert "STF opening taisen" $
        (mergeCombined >>> trimInfo) (analyzeSTFBattle dc12 combinedFleetOpeingTaisenSTF1) ==
          map toMaybeInt [50,50,45, 9999,9999,9999,
                          44,29,33, 9999,9999,9999,
                          33-77,33-78, 9999,9999,9999,9999]
      Assert.assert "CTF opening taisen" $
        (mergeCombined >>> trimInfo) (analyzeCTFBattle dc12 combinedFleetOpeingTaisenCTF1) ==
          map toMaybeInt [54,45,45, 9999,9999,9999,
                          44,47,29,33, 9999,9999,
                          33-89,33-90, 9999,9999,9999,9999]
      Assert.assert "combined night battle 1" $
        (merge >>> trimInfo) (analyzeCombinedNightBattle dc6 combinedFleetNightBattle1) ==
          map toMaybeInt [11-1-1, 4, 23, 75, 40-26, 21,
                          314-11-73, 217-29-19, 143-15-11-26-75,61-54-24, 0,0]
      Assert.assert "combined night battle 2" $
        (merge >>> trimInfo) (analyzeCombinedNightBattle dc6 combinedFleetNightBattle2) ==
          map toMaybeInt [81,34,13,39,17,28,
                          88-158-136,0,0,0,0, 9999]
      Assert.assert "abyssal combined fleet 1" $
        (mergeAC >>> trimInfo) (analyzeAbyssalCTFBattle dc6 abyssalCombinedFleet1) ==
          map toMaybeInt [96-9,38,60-4,50,16,23,
                          350-63-37-25,96-71-148,96-10-62-161,66-178,38-93,38-73,
                          57-232,76-5,76-5-4-58,38-122,35-101,35-176]
      Assert.assert "abyssal combined fleet night 1 (against main)" $
        (mergeAC >>> trimInfo) (analyzeAbyssalCTFNightBattle dc6 abyssalCombinedFleetNight1) ==
          map toMaybeInt [87,38,56,50,16,23,
                          225-91-42-31-71,0,0,0,0,0,
                          0,71,9,0,0,0]
      Assert.assert "abyssal combined fleet night 2 (against escort)" $
        (mergeAC >>> trimInfo) (analyzeAbyssalCTFNightBattle dc6 abyssalCombinedFleetNight2) ==
          map toMaybeInt [45,69,92,29,29,48,
                          142,27,53,21,0,0,
                          0,0,61-99-62,24-74-72,0,35-121-81]
      Assert.assert "both combined CTF 1" $
        (mergeBC >>> trimInfo) (analyzeBothCombinedCTFBattle dc12 bothCombinedCTF1) ==
          map toMaybeInt
            [67,57-25,83,52,57,45,
             50,35-4,31,52-6,53-7-13,43,
             370-34-167,88-154,88-91,80-79-98,35-65,35-133,
             57-261,76-116,76-7-237,55-55,20-234,20-71]
  where
    repairTeam = replicate 6 (Just RepairTeam)
    toMaybeInt x = if x == 9999 then Nothing else Just x
    merge x = x.main <> x.enemy
    mergeCombined x = x.main <> x.escort <> x.enemy
    mergeAC x = x.main <> x.enemyMain <> x.enemyEscort
    mergeBC x = x.allyMain <> x.allyEscort <> x.enemyMain <> x.enemyEscort    
    trimInfo :: forall a. Array (Maybe { hp :: Int | a}) -> Array (Maybe Int)
    trimInfo = (map <<< map) (\x -> x.hp)
