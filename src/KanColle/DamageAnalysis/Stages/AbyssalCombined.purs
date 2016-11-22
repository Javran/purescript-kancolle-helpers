module KanColle.DamageAnalysis.Stages.AbyssalCombined where

import KanColle.KCAPI.Battle

import Prelude

import KanColle.KCAPI.Battle.AbyssalCombined as AC

import KanColle.Util
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Stages.Internal

hougeki1DV :: Battle -> LR (LR DamageVector)
hougeki1DV = connectDV AC.getHougeki1 mt calcHougekiDamageAC

hougeki2DV :: Battle -> LR (LR DamageVector)
hougeki2DV = connectDV AC.getHougeki2 mt calcHougekiDamageAC

raigekiDV :: Battle -> LR (LR DamageVector)
raigekiDV = connectDV AC.getRaigeki mt calcRaigekiDamageAC

hougeki3DV :: Battle -> LR (LR DamageVector)
hougeki3DV = connectDV AC.getHougeki3 mt calcHougekiDamageAC

battleDV :: Battle -> CombinedDamageVectorAC
battleDV = fconcat2AC
    [ landBasedAirStrikeDVsAC
    , koukuDVAC
    -- support expedition: aerial or hourai
    , supportAirAttackDVAC
    , supportHouraiDVAC
    -- in case there are aerial battles:
    , kouku2DVAC

    -- preemptive anti-sub
    , openingTaisenDVAC

    -- regular battles
    , openingDVAC
    , hougeki1DV
    , raigekiDV
    , hougeki2DV
    , hougeki3DV
    ] >>> toCombinedAC
