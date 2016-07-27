module ExpeditionTests where

import Prelude
import Test.Unit
import Test.Unit.Assert
import Data.Array
import Data.Foldable

import KanColle.Expedition
import KanColle.Expedition.Base
import KanColle.Expedition.Requirement
import KanColle.Expedition.Minimal
import KanColle.SType

eqFleetReq :: FleetRequirement -> FleetRequirement -> Boolean
eqFleetReq r1 r2 = explainFleetRequirement r1 == explainFleetRequirement r2

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
testExpeditionHelper :: forall e. TestSuite e
testExpeditionHelper =
    test "ExpeditionHelper" do
      assert "fleet test 1" $
        null (unsatisfiedRequirements 2 testFleet1)
      assert "fleet test 2" $
        getAvailableExpeditions testFleet1 == [1,2,3,6,27]
      assert "fleet test 3" $
        getAvailableExpeditions testFleet2 == [1,2,3,6,11,12,33,34]
      assert "fleet test 3" $
        getAvailableExpeditions testFleet3 == [1,2,3,4,5,6,9,11,12,21,33,34]
      assert "fleet test 4" $
        getAvailableExpeditions testFleet4 == [1,2,3,4,5,6,7,8,9,11,12,13,14,16,17,21,33,34,37]
      assert "Expedition #32, 3 ships should also do" $
        null (unsatisfiedRequirements 32 testFleet5)

testExpeditionMinimal :: forall e. TestSuite e
testExpeditionMinimal =
    test "ExpeditionMinimalCost" $
      assert "minimal costs are likely to be achivable" $
        all verify allExpeditionIds
  where
    verify eId = checkExpedition eId fleet
      where
        fleet = case getExpeditionMinCost eId of
            ECost ec -> ec.fleet

