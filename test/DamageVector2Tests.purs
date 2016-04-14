module DamageVector2Tests where

import Prelude
import Test.Unit
import Data.Foreign

import qualified Data.String as Str
import qualified Data.Array.Unsafe as AU
import Data.Array
import Data.Monoid
import Data.Maybe
import Data.Function
import KanColle.DamageAnalysisFFI
import KanColle.DamageAnalysis2
import KanColle.DamageAnalysis.DamageVector2
import KanColle.DamageAnalysis.Stages2
import KanColle.DamageAnalysis.Damage
import Test.Unit.Assert as Assert

import Base
import BattleData

-- | convert a DamageVector to a simple string form
dvToStr :: DamageVector2 -> String
dvToStr (DV2 dv) = Str.joinWith "," (map (damageToInt >>> show) fD) <> " -- "
                <> Str.joinWith "," (map (damageToInt >>> show) eD)
  where
    fD = slice 0 6 dv
    eD = slice 6 12 dv

testDamageVector2 :: forall e. TestUnit e
testDamageVector2 = do
    test "DamageVector2: first aerial battle stage" do
      Assert.assert "sample battle1" $
          "0,0,0,4,0,0 -- 0,0,21,0,164,0" == dvToStr (koukuDV (unsafeFromForeign battle1))
      Assert.assert "sample battle2" $
          dvToStr mempty == dvToStr (koukuDV (unsafeFromForeign battle2))
      Assert.assert "sample aerialBattle1" $
          dvToStr mempty == dvToStr (koukuDV (unsafeFromForeign aerialBattle1))
      Assert.assert "sample withSupportExpedition1" $
          "15,0,0,7,8,0 -- 55,0,0,0,0,0"
              == dvToStr (koukuDV (unsafeFromForeign withSupportExpedition1))
    test "DamageVector2: support fleet attack stage" do
      let dvF = supportAirAttackDV <> supportHouraiDV
      Assert.assert "sample battle1" $
          dvToStr mempty == dvToStr (dvF (unsafeFromForeign battle1))
      Assert.assert "sample withSupportExpedition1" $
          "0,0,0,0,0,0 -- 0,29,0,0,101,0"
              == dvToStr (dvF (unsafeFromForeign withSupportExpedition1))
      Assert.assert "sample withSupportExpedition2" $
          "0,0,0,0,0,0 -- 0,70,20,132,40,0"
              == dvToStr (dvF (unsafeFromForeign withSupportExpedition2))

testDamageAnalyzer2 :: forall e. MyTest e
testDamageAnalyzer2 =
    test "DamageAnalyzer2" do
      Assert.assert "battle sample 1" $
        (merge >>> trimInfo) (analyzeBattle dc6 (unsafeFromForeign battle1)) ==
          map Just [75,75,75,75-4,77,79-9
                   ,84-127,60-95,60-21-98,53-213-145,35-164,35-159-157]
      Assert.assert "battle sample 2" $
        (merge >>> trimInfo) (analyzeBattle dc6 (unsafeFromForeign battle2)) ==
          map toMaybeInt [15,15-3,18,18,9999,9999
                         ,90-81-41,55-116,28,28-236,70-121,70-131]
      Assert.assert "night battle sample 1" $
        (merge >>> trimInfo) (analyzeNightBattle dc6 (unsafeFromForeign nightBattle1)) ==
          map toMaybeInt [15,12,18,18,9999,9999,0,0,28-94,0,0,0]
      Assert.assert "night battle sample 2" $
        (merge >>> trimInfo) (analyzeNightBattle dc6 (unsafeFromForeign nightBattle2)) ==
          map toMaybeInt [75,75,6,69,23,43,0,41-181,21-124,0,0,9999]
      Assert.assert "aerial battle sample 1" $
        (merge >>> trimInfo) (analyzeBattle dc6 (unsafeFromForeign aerialBattle1)) ==
          map toMaybeInt [37,31,31,32,32,44-6,96,70,60,20,20,20]
      Assert.assert "ld aerial battle sample 1" $
        (merge >>> trimInfo) (analyzeBattle dc6 (unsafeFromForeign ldAerialBattle1)) ==
          map toMaybeInt [31,56,56,42,33,29,500,9999,9999,9999,9999,9999]
  where
    toMaybeInt x = if x == 9999 then Nothing else Just x
    merge x = x.main <> x.enemy
    trimInfo :: forall a. Array (Maybe { hp :: Int | a}) -> Array (Maybe Int)
    trimInfo = (map <<< map) (\x -> x.hp)
