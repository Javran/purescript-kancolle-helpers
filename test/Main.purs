module Test.Main where

import Prelude
import Control.Monad.Eff.Console
import Test.Unit
import Data.Array

import KanColle.Expedition
import KanColle.SType

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

main = runTest do
  test "simple fleet tests" do
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
