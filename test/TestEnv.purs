module TestEnv
  ( module Prelude
  , module Data.Foreign
  , module Data.Maybe
  , module KanColle.KCAPI.Battle
  , module KanColle.DamageAnalysis
  , module KanColle.DamageAnalysis.DamageVector
  , module BattleData
  
  , module TestEnv
  )
where

-- this module is just for the purpose of
-- manually testing in psci
-- which exports as many modules as possible.

import Prelude

import Data.Foreign
import Data.Maybe

import KanColle.KCAPI.Battle
import KanColle.DamageAnalysis
import KanColle.DamageAnalysis.DamageVector

import BattleData

testKoukuCombined :: Maybe DamageVector
testKoukuCombined = map calcKoukuDamageCombined kk
  where
    kk = getKouku (unsafeFromForeign surfaceTaskForceBattle1)
