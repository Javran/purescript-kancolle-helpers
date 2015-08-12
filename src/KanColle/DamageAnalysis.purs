module KanColle.DamageAnalysis where

import Prelude
import Data.Maybe
import Data.Maybe.Unsafe
import KanColle.KCAPI.Battle
import Data.Array
import Data.Foreign
import Data.Int
import Data.Monoid
import Data.Monoid.Endo
import Data.Nullable
import Data.Foldable
import Data.String (joinWith)
import Math
import qualified Data.Array.Unsafe as AU
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Stages

-- information about damage took by a ship
type DamageTookInfo = {currentHp :: Int}

type DamageTookInfoNight = DamageTookInfo

pprDamageTookInfo :: DamageTookInfo -> String
pprDamageTookInfo dti
  = "DamageInfo {"
 <> "currentHp=" <> show dti.currentHp
 <> "}"

pprDamageTookInfoNight :: DamageTookInfoNight -> String
pprDamageTookInfoNight dti
  = "DamageInfoNight {"
 <> "currentHp=" <> show dti.currentHp
 <> "}"

pprFleetDamageTookInfo :: AllFleetInfo DamageTookInfo -> String
pprFleetDamageTookInfo = joinWith " | " <<< map (maybe "<N/A>" pprDamageTookInfo)

pprFleetDamageTookInfoNight :: AllFleetInfo DamageTookInfoNight -> String
pprFleetDamageTookInfoNight = joinWith " | " <<< map (maybe "<N/A>" pprDamageTookInfoNight)

-- invariant: the length is always 1+12,
-- index 0 is never used
-- index 1-6 for frient units
-- index 7-12 for enemies
type AllFleetInfo a = Array (Maybe a)

-- for all inner "Array (Maybe a)"
-- index 0 is never used
-- index 1-6 is for the current fleet
type CombinedFleetInfo a =
  { main :: Array (Maybe a)
  , escort :: Array (Maybe a)
  , enemy :: Array (Maybe a)
  }

-- initialize a battle, does nothing but setup currentHp
battleStart :: Battle -> AllFleetInfo DamageTookInfo
battleStart = (map <<< map) (\x -> {currentHp: x}) <<< getInitHps

battleCombinedStart :: Battle -> CombinedFleetInfo DamageTookInfo
battleCombinedStart b =
    { main: mk (slice 1 7 mainAndEnemy)
    , escort: mk escort
    , enemy: mk (slice 7 13 mainAndEnemy)
    }
  where
    -- convert to the right format & pad with "Nothing"
    mk = ([Nothing] <>) <<< (map <<< map) (\x -> {currentHp: x})
    mainAndEnemy = getInitHps b
    escort = AU.tail $ getInitHpsCombined b

analyzeBattle :: Battle -> AllFleetInfo DamageTookInfo
analyzeBattle = applyDamageVector <$> battleDV <*> battleStart

analyzeNightBattle :: Battle -> AllFleetInfo DamageTookInfoNight
analyzeNightBattle = applyDamageVector <$> nightBattleDV <*> battleStart

analyzeRawBattle :: Foreign -> AllFleetInfo DamageTookInfo
analyzeRawBattle = analyzeBattle <<< unsafeFromForeign

analyzeRawNightBattle :: Foreign -> AllFleetInfo DamageTookInfoNight
analyzeRawNightBattle = analyzeNightBattle <<< unsafeFromForeign

analyzeRawBattleJS :: Foreign -> Array (Nullable DamageTookInfo)
analyzeRawBattleJS = map toNullable <<< analyzeRawBattle

analyzeRawNightBattleJS :: Foreign -> Array (Nullable DamageTookInfoNight)
analyzeRawNightBattleJS = map toNullable <<< analyzeRawNightBattle

applyDamageVector :: DamageVector
                  -> AllFleetInfo DamageTookInfo
                  -> AllFleetInfo DamageTookInfo
applyDamageVector (DV dv) = zipWith combine dv
  where
    combine dmg = map (\x -> x {currentHp = x.currentHp - dmg})

applyCombinedDamageVector :: CombinedDamageVector
                          -> CombinedFleetInfo DamageTookInfo
                          -> CombinedFleetInfo DamageTookInfo
applyCombinedDamageVector (CDV cdv) nowhps =
    { main: phaseResult1Main
    , escort: phaseResult2Escort
    , enemy: phaseResult2Enemy
    }
  where
    -- note that damage accumulation is not accumulated in chronological order
    -- but this won't effect the final result
    -- phase 1: main fleet & enemy
    fleetInfo1 = [Nothing] <> AU.tail nowhps.main <> AU.tail nowhps.enemy
    phaseResult1 = applyDamageVector cdv.main fleetInfo1
    phaseResult1Main = [Nothing] <> slice 1 7 phaseResult1
    phaseResult1Enemy = [Nothing] <> slice 7 13 phaseResult1
    -- phase 2: escort fleet & enemy
    fleetInfo2 = [Nothing] <> AU.tail nowhps.escort <> AU.tail phaseResult1Enemy
    phaseResult2 = applyDamageVector cdv.escort fleetInfo2
    phaseResult2Escort = [Nothing] <> slice 1 7 phaseResult2
    phaseResult2Enemy = [Nothing] <> slice 7 13 phaseResult2

analyzeSurfaceTaskForceBattle :: Battle -> CombinedFleetInfo DamageTookInfo
analyzeSurfaceTaskForceBattle =
    applyCombinedDamageVector
      <$> battleSurfaceTaskForceDV
      <*> battleCombinedStart

analyzeRawSurfaceTaskForceBattle :: Foreign -> CombinedFleetInfo DamageTookInfo
analyzeRawSurfaceTaskForceBattle = analyzeSurfaceTaskForceBattle <<< unsafeFromForeign

analyzeRawSurfaceTaskForceBattleJS :: Foreign
                                   -> { main   :: Array (Nullable DamageTookInfo)
                                      , escort :: Array (Nullable DamageTookInfo)
                                      , enemy  :: Array (Nullable DamageTookInfo) }
analyzeRawSurfaceTaskForceBattleJS = cov <<< analyzeRawSurfaceTaskForceBattle
  where
    c = map toNullable
    cov v = { main: c v.main, escort: c v.escort, enemy: c v.enemy }

analyzeCarrierTaskForceBattle :: Battle -> CombinedFleetInfo DamageTookInfo
analyzeCarrierTaskForceBattle =
    applyCombinedDamageVector
      <$> battleCarrierTaskForceDV
      <*> battleCombinedStart

analyzeRawCarrierTaskForceBattle :: Foreign -> CombinedFleetInfo DamageTookInfo
analyzeRawCarrierTaskForceBattle = analyzeCarrierTaskForceBattle <<< unsafeFromForeign

analyzeRawCarrierTaskForceBattleJS :: Foreign
                                   -> { main   :: Array (Nullable DamageTookInfo)
                                      , escort :: Array (Nullable DamageTookInfo)
                                      , enemy  :: Array (Nullable DamageTookInfo) }
analyzeRawCarrierTaskForceBattleJS = cov <<< analyzeRawCarrierTaskForceBattle
  where
    c = map toNullable
    cov v = { main: c v.main, escort: c v.escort, enemy: c v.enemy }

-- for combined fleet's night battle, whose "nowhps_combined"
-- should be taken into account instead of "nowhps"
analyzeNightBattleCombined :: Battle -> AllFleetInfo DamageTookInfoNight
analyzeNightBattleCombined b = applyDamageVector (nightBattleDV b) nightBattleHps
  where
    nightBattleInfo = battleCombinedStart b
    nightBattleHps = [Nothing]
                   <> AU.tail (nightBattleInfo.escort)
                   <> AU.tail (nightBattleInfo.enemy)

analyzeRawNightBattleCombined :: Foreign -> AllFleetInfo DamageTookInfoNight
analyzeRawNightBattleCombined = analyzeNightBattleCombined <<< unsafeFromForeign

analyzeRawNightBattleCombinedJS :: Foreign -> Array (Nullable DamageTookInfoNight)
analyzeRawNightBattleCombinedJS = map toNullable <<< analyzeRawNightBattleCombined
