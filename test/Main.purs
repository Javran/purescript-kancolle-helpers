module Test.Main where

import Prelude
import Control.Monad.Eff.Console
import Test.Unit
import Test.Unit.Console hiding (print)
import Data.Array

import KanColle.Expedition
import KanColle.DamageAnalysis
import KanColle.SType
import Data.Foreign
import Data.Maybe

import BattleData

instance eqFleetRequirementForTest :: Eq FleetRequirement where
    eq r1 r2 = explainFleetRequirement r1 == explainFleetRequirement r2

mkShip :: SType -> Int -> Int -> Ship ()
mkShip st l d =
    { ammo: 0
    , morale: 0
    , stype: st
    , level: l
    , drumCount: d }

testFleet1 :: Fleet ()
testFleet1 =
    [ mkShip SS 18 0
    , mkShip SS 59 0
    , mkShip SSV 62 0
    , mkShip SSV 61 0
    ]

testFleet2 :: Fleet ()
testFleet2 =
    [ mkShip DD 31 0
    , mkShip DD 12 0
    , mkShip DD 12 0
    , mkShip DD 14 0
    ]

testFleet3 :: Fleet ()
testFleet3 =
    [ mkShip CL 33 1
    , mkShip DD 13 1
    , mkShip DD 12 1
    , mkShip DD 8 0
    , mkShip DD 14 1
    ]

testFleet4 :: Fleet ()
testFleet4 =
    [ mkShip DD 58 1
    , mkShip DD 31 1
    , mkShip DD 44 1
    , mkShip DD 39 1
    , mkShip DD 36 1
    , mkShip CL 41 1
    ]

type MyTest e = Test (testOutput :: TestOutput | e)

-- TODO: cover all possible expeditions
testExpeditionHelper :: forall e. MyTest e
testExpeditionHelper =
    test "ExpeditionHelper" do
      assert "fleet test 1" $
        unsatisfiedRequirements 2 testFleet1 == []
      assert "fleet test 2" $
        getAvailableExpeditions testFleet1 == [1,2,3,6,27]
      assert "fleet test 3" $
        getAvailableExpeditions testFleet2 == [1,2,3,6,11,12]
      assert "fleet test 3" $
        getAvailableExpeditions testFleet3 == [1,2,3,4,5,6,9,11,12,21]
      assert "fleet test 4" $
        getAvailableExpeditions testFleet4 == [1,2,3,4,5,6,7,8,9,11,12,13,14,16,17,21,37]

testDamageAnalyzer :: forall e. MyTest e
testDamageAnalyzer =
    test "DamageAnalyzer" do
      assert "battle sample 1" $
        trimInfo (analyzeBattle (unsafeFromForeign battle1)) == [Nothing] <>
          map Just [75,75,75,75-4,77,79-9
                   ,84-127,60-95,60-21-98,53-213-145,35-164,35-159-157]
      assert "battle sample 2" $
        trimInfo (analyzeBattle (unsafeFromForeign battle2)) ==
          map toMaybeInt [9999
                         ,15,15-3,18,18,9999,9999
                         ,90-81-41,55-116,28,28-236,70-121,70-131]
      assert "night battle sample 1" $
        trimInfo (analyzeNightBattle (unsafeFromForeign nightBattle1)) ==
          map toMaybeInt [9999
                         ,15,12,18,18,9999,9999,0,0,28-94,0,0,0]
      assert "aerial battle sample 1" $
        trimInfo (analyzeBattle (unsafeFromForeign aerialBattle1)) ==
          map toMaybeInt [9999
                         ,37,31,31,32,32,44-6,96,70,60,20,20,20]
  where
    toMaybeInt x = if x == 9999 then Nothing else Just x
    trimInfo :: forall a. Array (Maybe { currentHp:: Int | a}) -> Array (Maybe Int)
    trimInfo = (map <<< map) (\x -> x.currentHp)

main = runTest do
    testExpeditionHelper
    testDamageAnalyzer
