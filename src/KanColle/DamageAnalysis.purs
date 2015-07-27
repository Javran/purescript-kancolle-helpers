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
type DamageTookInfo =
  { currentHp :: Int
  }

type DamageTookInfoNight =
  { currentHp :: Int
  }

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
pprFleetDamageTookInfo xs = joinWith " | " $ map (maybe "<N/A>" pprDamageTookInfo) xs

pprFleetDamageTookInfoNight :: AllFleetInfo DamageTookInfoNight -> String
pprFleetDamageTookInfoNight xs = joinWith " | " $ map (maybe "<N/A>" pprDamageTookInfoNight) xs


-- invariant: the length is always 1+12,
-- index 0 is never used
-- index 1-6 for frient units
-- index 7-12 for enemies
type AllFleetInfo a = Array (Maybe a)

type DamageAnalyzer = Battle
                   -> AllFleetInfo DamageTookInfo
                   -> AllFleetInfo DamageTookInfo

noDamage :: Int -> DamageTookInfo
noDamage hp =
  { currentHp: hp
  }

-- initialize a battle, does nothing but setup currentHp
battleStart :: Battle -> AllFleetInfo DamageTookInfo
battleStart b = hpList
  where
    hpList = map fromRawHp b.api_nowhps
    fromRawHp -1 = Nothing
    fromRawHp v = Just (noDamage v)

analyzeBattle :: Battle -> AllFleetInfo DamageTookInfo
analyzeBattle = analyzeBattleAlt

analyzeNightBattle :: NightBattle -> AllFleetInfo DamageTookInfoNight
analyzeNightBattle = analyzeNightBattleAlt

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

analyzeBattleAlt :: Battle -> AllFleetInfo DamageTookInfo
analyzeBattleAlt = applyDamageVector <$> battleDV <*> battleStart

analyzeNightBattleAlt :: NightBattle -> AllFleetInfo DamageTookInfoNight
analyzeNightBattleAlt = applyDamageVector <$> nightBattleDV <*> battleStart
