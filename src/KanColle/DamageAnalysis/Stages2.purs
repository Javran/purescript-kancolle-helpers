module KanColle.DamageAnalysis.Stages2 where

import Prelude
import Data.Maybe
import Data.Monoid
import Data.Foldable

import KanColle.KCAPI.Battle
import KanColle.Util
import KanColle.DamageAnalysis.DamageVector2

mkDV :: forall a b. (Battle -> Maybe a) -> b -> (a -> b) -> Battle -> b
mkDV getData z calc b = maybe z calc (getData b)

memptyLR :: forall m. Monoid m => LR m
memptyLR = {left: mempty, right: mempty}

lrAppend :: forall m. Monoid m => LR m -> LR m -> LR m
lrAppend a b = { left: a.left <> b.left, right: a.right <> b.right }

lrOnlyLeft :: forall m. Monoid m => m -> LR m
lrOnlyLeft l = { left: l, right: mempty }

lrOnlyRight :: forall m. Monoid m => m -> LR m
lrOnlyRight r = { left: mempty, right: r }

-- | get `DamageVector` of kouku stage from battle data
-- | all the names in this module are kept consistent with functions in
-- | `KanColle.DamageAnalysis.DamageVector`.
koukuDV :: Battle -> LR DamageVector2
koukuDV = mkDV getKouku memptyLR calcKoukuDamage

koukuCombinedDV :: Battle -> DamageVector2
koukuCombinedDV = mkDV getKouku mempty calcKoukuDamageCombined

kouku2CombinedDV :: Battle -> DamageVector2
kouku2CombinedDV = mkDV getKouku2 mempty calcKoukuDamageCombined

supportAirAttackDV :: Battle -> DamageVector2
supportAirAttackDV = mkDV getSupportAirInfo mempty calcSupportAirAttackDamage

supportHouraiDV :: Battle -> DamageVector2
supportHouraiDV = mkDV getSupportHouraiInfo mempty calcSupportHouraiDamage

kouku2DV :: Battle -> LR DamageVector2
kouku2DV = mkDV getKouku2 memptyLR calcKoukuDamage

openingDV :: Battle -> LR DamageVector2
openingDV = mkDV getOpeningAttack memptyLR calcRaigekiDamage

hougeki1DV :: Battle -> LR DamageVector2
hougeki1DV = mkDV getHougeki1 memptyLR calcHougekiDamage

hougeki2DV :: Battle -> LR DamageVector2
hougeki2DV = mkDV getHougeki2 memptyLR calcHougekiDamage

hougeki3DV :: Battle -> LR DamageVector2
hougeki3DV = mkDV getHougeki3 memptyLR calcHougekiDamage

raigekiDV :: Battle -> LR DamageVector2
raigekiDV = mkDV getRaigeki memptyLR calcRaigekiDamage

-- specalized for Carrier Task Force
hougeki1CTDV :: Battle -> LR DamageVector2
hougeki1CTDV = mkDV getHougeki1CT memptyLR calcHougekiDamage

raigekiCTDV :: Battle -> LR DamageVector2
raigekiCTDV = mkDV getRaigekiCT memptyLR calcRaigekiDamage

hougeki2CTDV :: Battle -> LR DamageVector2
hougeki2CTDV = mkDV getHougeki2CT memptyLR calcHougekiDamage

hougeki3CTDV :: Battle -> LR DamageVector2
hougeki3CTDV = mkDV getHougeki3CT memptyLR calcHougekiDamage

hougekiDV :: Battle -> LR DamageVector2
hougekiDV = mkDV getHougeki memptyLR calcHougekiDamage

lrToNormal :: forall a. LR a -> NormalBattle a
lrToNormal x = { main: x.left, enemy: x.right }

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
battleDV :: Battle -> NormalDamageVector
battleDV = fconcat [ koukuDV, kouku2DV
                   , supportAirAttackDV >>> lrOnlyRight
                   , supportHouraiDV >>> lrOnlyRight
                   , openingDV
                   , hougeki1DV, hougeki2DV, hougeki3DV
                   , raigekiDV
                   ] >>> lrToNormal
                   
fconcat :: Array (Battle -> LR DamageVector2) -> Battle -> LR DamageVector2
fconcat xs b = foldl lrAppend memptyLR ((\f -> f b) <$> xs)

-- | get `DamageVector2` of a night battle
-- | a night battle involves only `hougeki` (shelling stage)
nightBattleDV :: Battle -> NormalDamageVector
nightBattleDV = hougekiDV >>> lrToNormal

memptyCombined :: forall m. Monoid m => CombinedBattle m
memptyCombined = { main: mempty, escort: mempty, enemy: mempty }

combinedAppend :: forall m. Monoid m => CombinedBattle m -> CombinedBattle m -> CombinedBattle m
combinedAppend a b = { main: a.main <> b.main
                     , escort: a.escort <> b.escort
                     , enemy: a.enemy <> b.enemy
                     }

fconcat2 :: Array (Battle -> CombinedDamageVector) -> Battle -> CombinedDamageVector
fconcat2 xs b = foldl combinedAppend memptyCombined ((\f -> f b) <$> xs)

battleSurfaceTaskForceDV :: Battle -> CombinedDamageVector
battleSurfaceTaskForceDV = fconcat2
    [ koukuDV >>> toCombined FRMain
    , koukuCombinedDV >>> lrOnlyLeft >>> toCombined FREscort
    , supportAirAttackDV >>> lrOnlyRight >>> toCombined FRSupport
    , supportHouraiDV >>> lrOnlyRight >>> toCombined FRSupport
    -- the following 2 for aerial battles
    , kouku2DV >>> toCombined FRMain
    , kouku2CombinedDV >>> lrOnlyLeft >>> toCombined FREscort
    -- the followings are for reguler battles
    , openingDV >>> toCombined FREscort
    , hougeki1DV >>> toCombined FRMain
    , hougeki2DV >>> toCombined FRMain
    , hougeki3DV >>> toCombined FREscort
    , raigekiDV >>> toCombined FREscort
    ]

battleCarrierTaskForceDV :: Battle -> CombinedDamageVector
battleCarrierTaskForceDV = fconcat2
    [  koukuDV >>> toCombined FRMain    
    ,  koukuCombinedDV >>> lrOnlyLeft >>> toCombined FREscort  
    ,  supportAirAttackDV >>> lrOnlyRight >>> toCombined FRSupport 
    ,  supportHouraiDV >>> lrOnlyRight >>> toCombined FRSupport 
    -- the following 2 for aerial battles
    ,  kouku2DV >>> toCombined FRMain    
    ,  kouku2CombinedDV >>> lrOnlyLeft >>> toCombined FREscort  
    -- the followings are for regular battles
    ,  openingDV >>> toCombined FREscort  
    ,  hougeki1CTDV >>> toCombined FREscort  
    ,  raigekiCTDV >>> toCombined FREscort  
    ,  hougeki2CTDV >>> toCombined FRMain    
    ,  hougeki3CTDV >>>  toCombined FRMain    
    ]
