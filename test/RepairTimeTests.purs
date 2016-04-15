module RepairTimeTests where

import Prelude
import Test.Unit
import Test.Unit.Assert
import Data.Array
import Data.Foldable

import KanColle.SType
import KanColle.RepairTime

testRepairTime :: forall e. TestUnit e
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
      assert "sample with full HP" $ dockingInSec AR 46 45 45 == 0
      assert "sample Hayasui" $ dockingInSec AO 2 19 43 == mmss 8 30
      assert "submarine rounding" $ dockingInSec SS 25 3 10 == mmss 12 27
    test "Facility time" do
      assert "sample 1" $ facilityInSec SS 72 1 15 == hhmmss 1 0 0
      assert "sample 2" $ facilityInSec SSV 75 1 18 == hhmmss 2 40 0
      assert "sample 3" $ facilityInSec CL 64 40 50 == hhmmss 1 20 0
      assert "sample 4" $ facilityInSec DD 42 21 24 == mmss 20 0
      assert "sample 5" $ facilityInSec CV 79 73 79 == hhmmss 2 0 0
      assert "sample with full HP" $ facilityInSec AR 46 45 45 == 0
