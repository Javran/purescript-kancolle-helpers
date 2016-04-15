module TestEnv
  ( module Prelude
  , module Data.Foreign
  , module Data.Maybe
  , module KanColle.KCAPI.Battle
  , module KanColle.Util
  , module BattleData
  , module KanColle.Expedition.Plan
  , module KanColle.Expedition.Minimal
  , module KanColle.SType
  , module KanColle.Generated.SType
  , module KanColle.RepairTime
  , module Control.Monad.Eff.Console
  , module MasterData
  , module KanColle.Remodel
  , module Debug.Trace
  , module TestEnv
  )
where

-- this module is just for the purpose of
-- manually testing in psci
-- which exports as many modules as possible.

import Prelude

import Data.Foreign
import Data.Maybe
import Data.Monoid
import Data.Array.ST
import Control.Monad.Eff.Console

import KanColle.KCAPI.Battle
import KanColle.Expedition.Plan
import KanColle.Expedition.Minimal
import KanColle.Generated.SType
import KanColle.SType
import KanColle.RepairTime
import KanColle.Remodel
import Debug.Trace

import BattleData
import MasterData

import Control.Monad.Eff
import KanColle.Util

testResult :: RemodelInfoMap
testResult = collectRemodelInfo (unsafeFromForeign masterData)

testResult2 :: RemodelGroupMap
testResult2 = generateRemodelGroups testResult

testResult3 :: OriginMap
testResult3 = generateOriginMap testResult2
