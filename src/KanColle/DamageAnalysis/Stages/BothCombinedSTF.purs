module KanColle.DamageAnalysis.Stages.BothCombinedSTF where

import KanColle.KCAPI.Battle

import Prelude

import KanColle.KCAPI.Battle.BothCombinedCTF as BSTF

import KanColle.Util
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Stages.Internal

hougeki1DV :: Battle -> LR (LR DamageVector)
hougeki1DV = connectDV BSTF.getHougeki1 mt calcHougekiDamageAC

hougeki2DV :: Battle -> LR (LR DamageVector)
hougeki2DV = connectDV BSTF.getHougeki2 mt calcHougekiDamageAC

raigekiDV :: Battle -> LR (LR DamageVector)
raigekiDV = connectDV BSTF.getRaigeki mt calcRaigekiDamageAC

hougeki3DV :: Battle -> LR (LR DamageVector)
hougeki3DV = connectDV BSTF.getHougeki3 mt calcHougekiDamageAC

battleDV :: Battle -> GCombinedDamageVector
battleDV = fconcat2AC
    [ landBasedAirStrikeDVsBC
    , koukuDVBC
    -- support expedition: aerial or hourai
    , supportAirAttackDVAC
    , supportHouraiDVAC
    -- in case there are aerial battles:
    , kouku2DVBC

    -- preemptive anti-sub
    , openingTaisenDVAC

    -- regular battles
    , openingDVAC
    , hougeki1DV
    , hougeki2DV
    , hougeki3DV
    , raigekiDV        
    ] >>> toCombinedBC
