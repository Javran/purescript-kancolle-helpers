module DamageVectorTests where

import Prelude
import Test.Unit
import Data.Foreign

import qualified Data.String as Str
import qualified Data.Array.Unsafe as AU
import Data.Array
import Data.Monoid

import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Stages

import Base
import BattleData

-- | convert a DamageVector to a simple string form
dvToStr :: DamageVector -> String
dvToStr (DV dv) = Str.joinWith "," (map show fD) <> " -- "
               <> Str.joinWith "," (map show eD)
  where
    fD = slice 1 7 dv
    eD = slice 7 13 dv

testDamageVector :: forall e. MyTest e
testDamageVector = do
    test "DamageVector: first aerial battle stage" do
      assert "sample battle1" $
          "0,0,0,4,0,0 -- 0,0,21,0,164,0" == dvToStr (koukuDV (unsafeFromForeign battle1))
      assert "sample battle2" $
          dvToStr mempty == dvToStr (koukuDV (unsafeFromForeign battle2))
      assert "sample aerialBattle1" $
          dvToStr mempty == dvToStr (koukuDV (unsafeFromForeign aerialBattle1))
      assert "sample withSupportExpedition1" $
          "15,0,0,7,8,0 -- 55,0,0,0,0,0"
              == dvToStr (koukuDV (unsafeFromForeign withSupportExpedition1))
    test "DamageVector: support fleet attack stage" do
      let dvF = supportAirAttackDV <> supportHouraiDV
      assert "sample battle1" $
          dvToStr mempty == dvToStr (dvF (unsafeFromForeign battle1))
      assert "sample withSupportExpedition1" $
          "0,0,0,0,0,0 -- 0,29,0,0,101,0"
              == dvToStr (dvF (unsafeFromForeign withSupportExpedition1))
      assert "sample withSupportExpedition2" $
          "0,0,0,0,0,0 -- 0,70,20,132,40,0"
              == dvToStr (dvF (unsafeFromForeign withSupportExpedition2))
