module KanColle.DamageAnalysis.Stages.BothCombinedCTF where

import KanColle.KCAPI.Battle

import Prelude
import Data.Maybe
import Data.Monoid
import Data.Foldable

import KanColle.KCAPI.Battle
import KanColle.KCAPI.Battle.BothCombinedCTF as BCTF

import KanColle.Util
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Types
import KanColle.DamageAnalysis.Stages.Internal

hougeki1DV :: Battle -> LR (LR DamageVector)
hougeki1DV = connectDV BCTF.getHougeki1 mt calcHougekiDamageAC

hougeki2DV :: Battle -> LR (LR DamageVector)
hougeki2DV = connectDV BCTF.getHougeki2 mt calcHougekiDamageAC

raigekiDV :: Battle -> LR (LR DamageVector)
raigekiDV = connectDV BCTF.getRaigeki mt calcRaigekiDamageAC

hougeki3DV :: Battle -> LR (LR DamageVector)
hougeki3DV = connectDV BCTF.getHougeki3 mt calcHougekiDamageAC

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
    , raigekiDV    
    , hougeki3DV
    ] >>> toCombinedBC
