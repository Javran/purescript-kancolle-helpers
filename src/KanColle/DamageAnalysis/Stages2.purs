module KanColle.DamageAnalysis.Stages2 where

import Prelude
import Data.Maybe
import Data.Monoid
import Data.Foldable

import KanColle.KCAPI.Battle
import KanColle.DamageAnalysis.DamageVector2

-- | get `DamageVector` of kouku stage from battle data
-- | all the names in this module are kept consistent with functions in
-- | `KanColle.DamageAnalysis.DamageVector`.
koukuDV :: Battle -> DamageVector2
koukuDV = foldMap calcKoukuDamage <<< getKouku

koukuCombinedDV :: Battle -> DamageVector2
koukuCombinedDV = foldMap calcKoukuDamageCombined <<< getKouku

kouku2CombinedDV :: Battle -> DamageVector2
kouku2CombinedDV = foldMap calcKoukuDamageCombined <<< getKouku2

supportAirAttackDV :: Battle -> DamageVector2
supportAirAttackDV = foldMap calcSupportAirAttackDamage <<< getSupportAirInfo

supportHouraiDV :: Battle -> DamageVector2
supportHouraiDV = foldMap calcSupportHouraiDamage <<< getSupportHouraiInfo

kouku2DV :: Battle -> DamageVector2
kouku2DV = foldMap calcKoukuDamage <<< getKouku2

openingDV :: Battle -> DamageVector2
openingDV = foldMap calcRaigekiDamage <<< getOpeningAttack

hougeki1DV :: Battle -> DamageVector2
hougeki1DV = foldMap calcHougekiDamage <<< getHougeki1

hougeki2DV :: Battle -> DamageVector2
hougeki2DV = foldMap calcHougekiDamage <<< getHougeki2

hougeki3DV :: Battle -> DamageVector2
hougeki3DV = foldMap calcHougekiDamage <<< getHougeki3

raigekiDV :: Battle -> DamageVector2
raigekiDV = foldMap calcRaigekiDamage <<< getRaigeki

-- specalized for Carrier Task Force
hougeki1CTDV :: Battle -> DamageVector2
hougeki1CTDV = foldMap calcHougekiDamage <<< getHougeki1CT

raigekiCTDV :: Battle -> DamageVector2
raigekiCTDV = foldMap calcRaigekiDamage <<< getRaigekiCT

hougeki2CTDV :: Battle -> DamageVector2
hougeki2CTDV = foldMap calcHougekiDamage <<< getHougeki2CT

hougeki3CTDV :: Battle -> DamageVector2
hougeki3CTDV = foldMap calcHougekiDamage <<< getHougeki3CT

hougekiDV :: Battle -> DamageVector2
hougekiDV = foldMap calcHougekiDamage <<< getHougeki

-- | get `DamageVector2` of a regular / aerial battle from battle data
-- | a regular battle consists of the following stages:
-- |
-- | * `kouku`  (aerial battle)
-- | * `kouku2` (aerial battle)
-- | * `opening` (openning torpedo attack)
-- | * `hougeki1` (first shelling stage)
-- | * `hougeki2` (second shelling stage)
-- | * `hougeki3` (third shelling stage, always empty for now)
-- | * `raigeki` (closing torpedo attack)
battleDV :: Battle -> DamageVector2
battleDV = mconcat [ koukuDV, kouku2DV
                   , supportAirAttackDV, supportHouraiDV
                   , openingDV
                   , hougeki1DV, hougeki2DV, hougeki3DV
                   , raigekiDV
                   ]

-- | get `DamageVector2` of a night battle
-- | a night battle involves only `hougeki` (shelling stage)
nightBattleDV :: Battle -> DamageVector2
nightBattleDV = hougekiDV

battleSurfaceTaskForceDV :: Battle -> CombinedDamageVector2
battleSurfaceTaskForceDV = mconcat
    [ toCombined FRMain    <<< koukuDV
    , toCombined FREscort  <<< koukuCombinedDV
    , toCombined FRSupport <<< supportAirAttackDV
    , toCombined FRSupport <<< supportHouraiDV
    -- the following 2 for aerial battles
    , toCombined FRMain    <<< kouku2DV
    , toCombined FREscort  <<< kouku2CombinedDV
    -- the followings are for reguler battles
    , toCombined FREscort  <<< openingDV
    , toCombined FRMain    <<< hougeki1DV
    , toCombined FRMain    <<< hougeki2DV
    , toCombined FREscort  <<< hougeki3DV
    , toCombined FREscort  <<< raigekiDV
    ]

battleCarrierTaskForceDV :: Battle -> CombinedDamageVector2
battleCarrierTaskForceDV = mconcat
    [ toCombined FRMain    <<< koukuDV
    , toCombined FREscort  <<< koukuCombinedDV
    , toCombined FRSupport <<< supportAirAttackDV
    , toCombined FRSupport <<< supportHouraiDV
    -- the following 2 for aerial battles
    , toCombined FRMain    <<< kouku2DV
    , toCombined FREscort  <<< kouku2CombinedDV
    -- the followings are for regular battles
    , toCombined FREscort  <<< openingDV
    , toCombined FREscort  <<< hougeki1CTDV
    , toCombined FREscort  <<< raigekiCTDV
    , toCombined FRMain    <<< hougeki2CTDV
    , toCombined FRMain    <<< hougeki3CTDV
    ]
