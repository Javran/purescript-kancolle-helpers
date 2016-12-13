module KanColle.DamageAnalysis.Stages.CTF where

-- TODO: regular CTF / STF's behavior is still unknown under jet assault
-- for now we have no way to verify though.

import KanColle.KCAPI.Battle

import Prelude
import Data.Maybe
import Data.Monoid
import Data.Foldable

import KanColle.KCAPI.Battle
import KanColle.KCAPI.Battle.CTF as CTF

import KanColle.Util
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Types
import KanColle.DamageAnalysis.Stages.Internal

hougeki1DV :: Battle -> LR DamageVector
hougeki1DV = connectDV CTF.getHougeki1 memptyLR calcHougekiDamage

raigekiDV :: Battle -> LR DamageVector
raigekiDV = connectDV CTF.getRaigeki memptyLR calcRaigekiDamage

hougeki2DV :: Battle -> LR DamageVector
hougeki2DV = connectDV CTF.getHougeki2 memptyLR calcHougekiDamage

hougeki3DV :: Battle -> LR DamageVector
hougeki3DV = connectDV CTF.getHougeki3 memptyLR calcHougekiDamage

-- | get `CombinedDamageVector` of a carrier task force battle
-- | note that transport escort battle uses this `CombinedDamageVector` as well
battleDV :: Battle -> CombinedDamageVector
battleDV = fconcat2
    [ landBasedAirStrikeDVs >>> toCombined FRLandBased
    , koukuDV >>> toCombined FRMain
    , koukuCombinedDV >>> lrOnlyLeft >>> toCombined FREscort
    , supportAirAttackDV >>> lrOnlyRight >>> toCombined FRSupport
    , supportHouraiDV >>> lrOnlyRight >>> toCombined FRSupport
    -- the following 2 for aerial battles
    , kouku2DV >>> toCombined FRMain
    , kouku2CombinedDV >>> lrOnlyLeft >>> toCombined FREscort

    -- only escort fleet will do preemptive anti-sub
    , openingTaisenDV >>> toCombined FREscort

    -- the followings are for regular battles
    , openingDV >>> toCombined FREscort
    , hougeki1DV >>> toCombined FREscort
    , raigekiDV >>> toCombined FREscort
    , hougeki2DV >>> toCombined FRMain
    , hougeki3DV >>>  toCombined FRMain
    ]
