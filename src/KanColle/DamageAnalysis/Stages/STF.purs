module KanColle.DamageAnalysis.Stages.STF where

-- TODO: see CTF.purs

import Prelude

import KanColle.KCAPI.Battle
import KanColle.KCAPI.Battle.STF as STF

import KanColle.Util
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Stages.Internal

hougeki1DV :: Battle -> LR DamageVector
hougeki1DV = connectDV STF.getHougeki1 memptyLR calcHougekiDamage

raigekiDV :: Battle -> LR DamageVector
raigekiDV = connectDV STF.getRaigeki memptyLR calcRaigekiDamage

hougeki2DV :: Battle -> LR DamageVector
hougeki2DV = connectDV STF.getHougeki2 memptyLR calcHougekiDamage

hougeki3DV :: Battle -> LR DamageVector
hougeki3DV = connectDV STF.getHougeki3 memptyLR calcHougekiDamage

-- | get `CombinedDamageVector` of a surface task force battle
battleDV :: Battle -> CombinedDamageVector
battleDV = fconcat2
    [ injLandBasedDV >>> toCombined FRLandBased
    , injKoukuDV >>> toCombined FRMain
    , landBasedAirStrikeDVs >>> toCombined FRLandBased
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
    , hougeki1DV >>> toCombined FRMain
    , hougeki2DV >>> toCombined FRMain
    , hougeki3DV >>> toCombined FREscort
    , raigekiDV >>> toCombined FREscort
    ]
