{- | This module implements attacking stages of battles
   | which accumulates damages values in the correct order
   | to construct `DamageVector`s for all damage-taking fleets involved in
   | the battle.
 -}
module KanColle.DamageAnalysis.Stages
  ( koukuDV
  , koukuDVAC
  , koukuCombinedDV
  , battleDV
  , nightBattleDV
  , supportAirAttackDV
  , supportHouraiDV
  , landBasedAirStrikeDVsAC

  , battleCarrierTaskForceDV
  , battleSurfaceTaskForceDV
  , battleEnemyCarrierTaskForceDV
  
  , hougeki1BCDV
  , hougeki2BCDV  
  , hougeki3BCDV
  , raigekiBCDV
  ) where

import Prelude
import Data.Maybe
import Data.Monoid
import Data.Foldable

import KanColle.KCAPI.Battle
import KanColle.Util
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Types

connectDV :: forall a b. (Battle -> Maybe a) -> b -> (a -> b) -> Battle -> b
connectDV getData z calc b = maybe z calc (getData b)

-- | get `DamageVector` of kouku stage from battle data
-- | all the names in this module are kept consistent with functions in
-- | `KanColle.DamageAnalysis.DamageVector`.
koukuDV :: Battle -> LR DamageVector
koukuDV = connectDV getKouku memptyLR calcKoukuDamage

koukuDVAC :: Battle -> LR (LR DamageVector)
koukuDVAC = connectDV getKouku mt calcKoukuDamageAC

koukuDVBC :: Battle -> LR (LR DamageVector)
koukuDVBC = koukuDVAC

koukuCombinedDV :: Battle -> DamageVector
koukuCombinedDV = connectDV getKouku mempty calcKoukuDamageCombined

landBasedAirStrikeDVs :: Battle -> LR DamageVector
landBasedAirStrikeDVs =
    connectDV
      getLandBasedAirStrikes
      []
      (map (calcLandBasedKoukuDamage >>> lrOnlyRight))
    >>> foldl lrAppend memptyLR

landBasedAirStrikeDVsAC :: Battle -> LR (LR DamageVector)
landBasedAirStrikeDVsAC =
    connectDV
      getLandBasedAirStrikes
      []
      (map calcKoukuDamageAC)
    >>> foldl auxAppend mt
  where
    auxAppend l r =
        { left: lrAppend l.left r.left
        , right: lrAppend l.right r.right }

landBasedAirStrikeDVsBC :: Battle -> LR (LR DamageVector)
landBasedAirStrikeDVsBC = landBasedAirStrikeDVsAC

kouku2CombinedDV :: Battle -> DamageVector
kouku2CombinedDV = connectDV getKouku2 mempty calcKoukuDamageCombined

supportAirAttackDV :: Battle -> DamageVector
supportAirAttackDV = connectDV getSupportAirInfo mempty calcSupportAirAttackDamage

supportAirAttackDVAC :: Battle -> LR (LR DamageVector)
supportAirAttackDVAC = connectDV getSupportAirInfo mt calcSupportAirAttackDamageAC

supportHouraiDV :: Battle -> DamageVector
supportHouraiDV = connectDV getSupportHouraiInfo mempty calcSupportHouraiDamage

supportHouraiDVAC :: Battle -> LR (LR DamageVector)
supportHouraiDVAC = connectDV getSupportHouraiInfo mt calcSupportHouraiDamageAC

kouku2DV :: Battle -> LR DamageVector
kouku2DV = connectDV getKouku2 memptyLR calcKoukuDamage

kouku2DVAC :: Battle -> LR (LR DamageVector)
kouku2DVAC = connectDV getKouku2 mt calcKoukuDamageAC

kouku2DVBC :: Battle -> LR (LR DamageVector)
kouku2DVBC = kouku2DVAC

openingDV :: Battle -> LR DamageVector
openingDV = connectDV getOpeningAttack memptyLR calcRaigekiDamage

openingDVAC :: Battle -> LR (LR DamageVector)
openingDVAC = connectDV getOpeningAttack mt calcRaigekiDamageAC

openingTaisenDV :: Battle -> LR DamageVector
openingTaisenDV = connectDV getOpeningTaisen memptyLR calcHougekiDamage

openingTaisenDVAC :: Battle -> LR (LR DamageVector)
openingTaisenDVAC = connectDV getOpeningTaisen mt calcHougekiDamageAC

hougeki1DV :: Battle -> LR DamageVector
hougeki1DV = connectDV getHougeki1 memptyLR calcHougekiDamage

hougeki2DV :: Battle -> LR DamageVector
hougeki2DV = connectDV getHougeki2 memptyLR calcHougekiDamage

hougeki3DV :: Battle -> LR DamageVector
hougeki3DV = connectDV getHougeki3 memptyLR calcHougekiDamage

raigekiDV :: Battle -> LR DamageVector
raigekiDV = connectDV getRaigeki memptyLR calcRaigekiDamage

-- specalized for Carrier Task Force
hougeki1CTDV :: Battle -> LR DamageVector
hougeki1CTDV = connectDV getHougeki1CT memptyLR calcHougekiDamage

raigekiCTDV :: Battle -> LR DamageVector
raigekiCTDV = connectDV getRaigekiCT memptyLR calcRaigekiDamage

hougeki2CTDV :: Battle -> LR DamageVector
hougeki2CTDV = connectDV getHougeki2CT memptyLR calcHougekiDamage

hougeki3CTDV :: Battle -> LR DamageVector
hougeki3CTDV = connectDV getHougeki3CT memptyLR calcHougekiDamage

mt :: LR (LR DamageVector)
mt = { left: memptyLR, right: memptyLR }

-- specalized for Abyssal Combined Fleet
hougeki1ACDV :: Battle -> LR (LR DamageVector)
hougeki1ACDV = connectDV getHougeki1CT mt calcHougekiDamageAC

raigekiACDV :: Battle -> LR (LR DamageVector)
raigekiACDV = connectDV getRaigekiCT mt calcRaigekiDamageAC

hougeki2ACDV :: Battle -> LR (LR DamageVector)
hougeki2ACDV = connectDV getHougeki2CT mt calcHougekiDamageAC

hougeki3ACDV :: Battle -> LR (LR DamageVector)
hougeki3ACDV = connectDV getHougeki3CT mt calcHougekiDamageAC

hougeki1BCDV :: Battle -> LR (LR DamageVector)
hougeki1BCDV = connectDV getHougeki1BC mt calcHougekiDamageAC

raigekiBCDV :: Battle -> LR (LR DamageVector)
raigekiBCDV = connectDV getRaigekiBC mt calcRaigekiDamageAC

hougeki2BCDV :: Battle -> LR (LR DamageVector)
hougeki2BCDV = connectDV getHougeki2BC mt calcHougekiDamageAC

hougeki3BCDV :: Battle -> LR (LR DamageVector)
hougeki3BCDV = connectDV getHougeki3BC mt calcHougekiDamageAC

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
battleDV = fconcat [ landBasedAirStrikeDVs
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

fconcat :: Array (Battle -> LR DamageVector) -> Battle -> LR DamageVector
fconcat xs b = foldl lrAppend memptyLR ((\f -> f b) <$> xs)

-- | get `NormalDamageVector` of a night battle
-- | a night battle involves only `hougeki` (shelling stage)
nightBattleDV :: Battle -> NormalDamageVector
nightBattleDV = hougekiDV >>> lrToNormal

memptyCombined :: forall m. Monoid m => CombinedBattle m
memptyCombined = { main: mempty, escort: mempty, enemy: mempty }

memptyCombinedAC :: forall m. Monoid m => CombinedBattleAC m
memptyCombinedAC = { main: mempty, enemyMain: mempty, enemyEscort: mempty }

combinedAppend :: forall m. Monoid m => CombinedBattle m -> CombinedBattle m -> CombinedBattle m
combinedAppend a b = { main: a.main <> b.main
                     , escort: a.escort <> b.escort
                     , enemy: a.enemy <> b.enemy
                     }

combinedAppendAC :: forall m. Monoid m => CombinedBattleAC m -> CombinedBattleAC m -> CombinedBattleAC m
combinedAppendAC a b =
    { main: a.main <> b.main
    , enemyMain: a.enemyMain <> b.enemyMain
    , enemyEscort: a.enemyEscort <> b.enemyEscort
    }

fconcat2 :: Array (Battle -> CombinedDamageVector) -> Battle -> CombinedDamageVector
fconcat2 xs b = foldl combinedAppend memptyCombined ((\f -> f b) <$> xs)


-- | get `CombinedDamageVector` of a surface task force battle
battleSurfaceTaskForceDV :: Battle -> CombinedDamageVector
battleSurfaceTaskForceDV = fconcat2
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
    , hougeki1DV >>> toCombined FRMain
    , hougeki2DV >>> toCombined FRMain
    , hougeki3DV >>> toCombined FREscort
    , raigekiDV >>> toCombined FREscort
    ]

-- | get `CombinedDamageVector` of a carrier task force battle
-- | note that transport escort battle uses this `CombinedDamageVector` as well
battleCarrierTaskForceDV :: Battle -> CombinedDamageVector
battleCarrierTaskForceDV = fconcat2
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
    , hougeki1CTDV >>> toCombined FREscort
    , raigekiCTDV >>> toCombined FREscort
    , hougeki2CTDV >>> toCombined FRMain
    , hougeki3CTDV >>>  toCombined FRMain
    ]

fconcat2AC :: Array (Battle -> LR (LR DamageVector)) -> Battle -> LR (LR DamageVector)
fconcat2AC xs b = foldl auxAppend mt ((\f -> f b) <$> xs)
  where
    auxAppend l r =
        { left: lrAppend l.left r.left
        , right: lrAppend l.right r.right }

-- TODO: to be verified
battleEnemyCarrierTaskForceDV :: Battle -> CombinedDamageVectorAC
battleEnemyCarrierTaskForceDV = fconcat2AC
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
    , hougeki1ACDV
    , raigekiACDV
    , hougeki2ACDV
    , hougeki3ACDV
    ] >>> toCombinedAC

-- TODO: quick and dirty solution
-- BC and AC can share a lot of implementation,
-- but the following code looks messy, we'd better find a way to clean it up
battleBothCombinedCarrierTaskForceDV :: Battle -> GCombinedDamageVector
battleBothCombinedCarrierTaskForceDV = fconcat2AC
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
    , openingTaisenDVAC

    -- regular battles
    , openingDVAC
    , hougeki1BCDV
    , hougeki2BCDV
    , raigekiBCDV    
    , hougeki3BCDV
    ] >>> toCombinedBC
