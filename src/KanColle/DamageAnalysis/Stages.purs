{- | This module implements attacking stages of battles
   | which accumulates damages values in the correct order
   | to construct `DamageVector`s for all damage-taking fleets involved in
   | the battle.
 -}
module KanColle.DamageAnalysis.Stages
  ( battleDV
  , nightBattleDV
  ) where

import Prelude
import Data.Maybe
import Data.Monoid
import Data.Foldable

import KanColle.KCAPI.Battle

import KanColle.Util
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Types
import KanColle.DamageAnalysis.Stages.Internal

hougeki1DV :: Battle -> LR DamageVector
hougeki1DV = connectDV getHougeki1 memptyLR calcHougekiDamage

hougeki2DV :: Battle -> LR DamageVector
hougeki2DV = connectDV getHougeki2 memptyLR calcHougekiDamage

hougeki3DV :: Battle -> LR DamageVector
hougeki3DV = connectDV getHougeki3 memptyLR calcHougekiDamage

raigekiDV :: Battle -> LR DamageVector
raigekiDV = connectDV getRaigeki memptyLR calcRaigekiDamage

hougekiDV :: Battle -> LR DamageVector
hougekiDV = connectDV getHougeki memptyLR calcHougekiDamage

-- | get `NormalDamageVector` of a regular / aerial battle from battle data
-- | a regular battle consists of the following stages:
-- |
-- | * `kouku`  (aerial battle)
-- | * `kouku2` (aerial battle)
-- | * `supportAirInfo` (airstrike from support expedition)
-- | * `supportHouraiInfo` (shelling attack from support expedition)
-- | * `opening` (openning torpedo attack)
-- | * `hougeki1` (first shelling stage)
-- | * `hougeki2` (second shelling stage)
-- | * `hougeki3` (third shelling stage, always empty for regular battles)
-- | * `raigeki` (closing torpedo attack)
battleDV :: Battle -> NormalDamageVector
battleDV = fconcat [ injLandBasedDV
                   , landBasedAirStrikeDVs
                   , injKoukuDV
                   , koukuDV, kouku2DV
                   , supportAirAttackDV >>> lrOnlyRight
                   , supportHouraiDV >>> lrOnlyRight
                     -- TODO: before or after support shelling?
                     -- for now it doesn't matter because subs cannot be attacked during this phase,
                     -- but we'd better make it right.
                     -- TODO: same for combined fleets, we can only figure it out during events ...
                   , openingTaisenDV
                   , openingDV
                   , hougeki1DV, hougeki2DV, hougeki3DV
                   , raigekiDV
                   ] >>> lrToNormal

-- | get `NormalDamageVector` of a night battle
-- | a night battle involves only `hougeki` (shelling stage)
nightBattleDV :: Battle -> NormalDamageVector
nightBattleDV = hougekiDV >>> lrToNormal
