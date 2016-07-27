module Test.Main where

import Prelude
import Test.Unit
import Test.Unit.Main
import Control.Monad.Eff

import DamageVectorTests
import ExpeditionTests
import RepairTimeTests

main :: Eff _ Unit
main = do
  -- NOTE: make sure to run "unitTests" after every other
  -- tests are done, for now everything after it doesn't
  -- seem to be executed
  runTest do
    testExpeditionHelper
    testDameCon
    testDamageVector  
    testDamageAnalyzer
    testExpeditionMinimal
    testRepairTime
