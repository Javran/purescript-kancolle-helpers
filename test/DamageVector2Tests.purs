module DamageVector2Tests where

import Prelude
import Test.Unit
import Data.Foreign

import qualified Data.String as Str
import qualified Data.Array.Unsafe as AU
import Data.Array
import Data.Monoid

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
