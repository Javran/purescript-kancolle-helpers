module Test.Main where

import Prelude
import Control.Monad.Eff.Console
import Test.Unit
import Test.Unit.Console (TestOutput(..))
import Data.Array
import Data.Foldable
import Control.Monad.Eff

import KanColle.Expedition
import KanColle.Expedition.Requirement
import KanColle.Expedition.Minimal
import KanColle.DamageAnalysis
import KanColle.DamageAnalysis.DamageVector
import KanColle.KCAPI.Battle
import KanColle.SType
import KanColle.RepairTime
import Data.Foreign
import Data.Maybe

import DamageVectorTests
import UtilTests
import BattleData
import Base
import qualified Test.QuickCheck as QC

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

testFleet5 :: Fleet ()
testFleet5 =
    [ mkShip CT 5 0
    , mkShip DD 1 0
    , mkShip DD 1 0
    ]

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
      assert "Expedition #32, 3 ships should also do" $
        unsatisfiedRequirements 32 testFleet5 == []

-- TODO: break into stages
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
      assert "night battle sample 2" $
        trimInfo (analyzeNightBattle (unsafeFromForeign nightBattle2)) ==
          map toMaybeInt [9999
                         ,75,75,6,69,23,43,0,41-181,21-124,0,0,9999]
      assert "aerial battle sample 1" $
        trimInfo (analyzeBattle (unsafeFromForeign aerialBattle1)) ==
          map toMaybeInt [9999
                         ,37,31,31,32,32,44-6,96,70,60,20,20,20]
  where
    toMaybeInt x = if x == 9999 then Nothing else Just x
    trimInfo :: forall a. Array (Maybe { currentHp:: Int | a}) -> Array (Maybe Int)
    trimInfo = (map <<< map) (\x -> x.currentHp)

testExpeditionMinimal :: forall e. MyTest e
testExpeditionMinimal =
    test "ExpeditionMinimalCost" $
      assert "minimal costs are likely to be achivable" $
        all verify expeditionIds
  where
    verify eId = checkExpedition eId fleet
      where
        fleet = case getExpeditionMinCost eId of
            ECost ec -> ec.fleet

testRepairTime :: forall e. MyTest e
testRepairTime = do
    let hhmmss h m s = h*3600 + m*60 + s
        mmss m s = m*60 + s
    test "Docking time" do
      assert "sample 1" $ dockingInSec SS 72 1 15 == mmss 56 30
      assert "sample 2" $ dockingInSec SSV 75 1 18 == hhmmss 2 23 35
      assert "sample 3" $ dockingInSec CL 64 40 50 == hhmmss 1 13 50
      assert "sample 4" $ dockingInSec DD 42 21 24 == mmss 16 0
      assert "sample 5" $ dockingInSec CV 79 73 79 == hhmmss 1 45 30
      assert "sample 6" $ dockingInSec DD 69 3 31 == hhmmss 3 37 30
      assert "sample 7" $ dockingInSec FBB 80 26 83 == hhmmss 12 35 45
      assert "sample 8" $ dockingInSec CAV 72 45 58 == hhmmss 2 36 30
      assert "sample 9" $ dockingInSec CV 79 70 77 == hhmmss 2 3 0
    test "Facility time" do
      assert "sample 1" $ facilityInSec SS 72 1 15 == hhmmss 1 0 0
      assert "sample 2" $ facilityInSec SSV 75 1 18 == hhmmss 2 40 0
      assert "sample 3" $ facilityInSec CL 64 40 50 == hhmmss 1 20 0
      assert "sample 4" $ facilityInSec DD 42 21 24 == mmss 20 0
      assert "sample 5" $ facilityInSec CV 79 73 79 == hhmmss 2 0 0

unitTests ::  forall e. Eff (testOutput :: TestOutput | e) Unit
unitTests =
    runTest $ do
        testExpeditionHelper
        testDamageVector
        testDamageAnalyzer
        testExpeditionMinimal
        testRepairTime

main = do
  qcTestUtils
  -- NOTE: make sure to run "unitTests" after every other
  -- tests are done, for now everything after it doesn't
  -- seem to be executed
  unitTests
