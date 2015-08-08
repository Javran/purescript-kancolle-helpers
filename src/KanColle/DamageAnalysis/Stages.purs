module KanColle.DamageAnalysis.Stages where

import Prelude
import Data.Maybe
import Data.Monoid
import Data.Foldable

import KanColle.KCAPI.Battle
import KanColle.DamageAnalysis.DamageVector

-- | get `DamageVector` of kouku stage from battle data
-- | all the names in this module are kept consistent with functions in
-- | `KanColle.DamageAnalysis.DamageVector`.
koukuDV :: Battle -> DamageVector
koukuDV = foldMap calcKoukuDamage <<< getKouku

supportAirAttackDV :: Battle -> DamageVector
supportAirAttackDV = foldMap calcSupportAirAttackDamage <<< getSupportAirInfo

supportHouraiDV :: Battle -> DamageVector
supportHouraiDV = foldMap calcSupportHouraiDamage <<< getSupportHouraiInfo

kouku2DV :: Battle -> DamageVector
kouku2DV = foldMap calcKoukuDamage <<< getKouku2

openingDV :: Battle -> DamageVector
openingDV = foldMap calcRaigekiDamage <<< getOpeningAttack

hougeki1DV :: Battle -> DamageVector
hougeki1DV = foldMap calcHougekiDamage <<< getHougeki1

hougeki2DV :: Battle -> DamageVector
hougeki2DV = foldMap calcHougekiDamage <<< getHougeki2

hougeki3DV :: Battle -> DamageVector
hougeki3DV = foldMap calcHougekiDamage <<< getHougeki3

raigekiDV :: Battle -> DamageVector
raigekiDV = foldMap calcRaigekiDamage <<< getRaigeki

hougekiDV :: Battle -> DamageVector
hougekiDV = foldMap calcHougekiDamage <<< getHougeki

-- | get `DamageVector` of a regular / aerial battle from battle data
-- | a regular battle consists of the following stages:
-- |
-- | * `kouku`  (aerial battle)
-- | * `kouku2` (aerial battle)
-- | * `opening` (openning torpedo attack)
-- | * `hougeki1` (first shelling stage)
-- | * `hougeki2` (second shelling stage)
-- | * `hougeki3` (third shelling stage, always empty for now)
-- | * `raigeki` (closing torpedo attack)
battleDV :: Battle -> DamageVector
battleDV = mconcat [ koukuDV, kouku2DV
                   , supportAirAttackDV, supportHouraiDV
                   , openingDV
                   , hougeki1DV, hougeki2DV, hougeki3DV
                   , raigekiDV
                   ]

-- | get `DamageVector` of a night battle
-- | a night battle involves only `hougeki` (shelling stage)
nightBattleDV :: Battle -> DamageVector
nightBattleDV = hougekiDV
