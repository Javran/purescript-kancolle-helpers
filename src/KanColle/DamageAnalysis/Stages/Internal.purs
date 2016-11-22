module KanColle.DamageAnalysis.Stages.Internal where

import Prelude
import Data.Maybe
import Data.Monoid
import Data.Foldable

import KanColle.KCAPI.Battle

import KanColle.Util
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Types

landBasedAirStrikeDVs :: Battle -> LR DamageVector
landBasedAirStrikeDVs =
    connectDV
      getLandBasedAirStrikes
      []
      (map (calcLandBasedKoukuDamage >>> lrOnlyRight))
    >>> foldl lrAppend memptyLR
    
-- | get `DamageVector` of kouku stage from battle data
-- | all the names in this module are kept consistent with functions in
-- | `KanColle.DamageAnalysis.DamageVector`.
koukuDV :: Battle -> LR DamageVector
koukuDV = connectDV getKouku memptyLR calcKoukuDamage

koukuCombinedDV :: Battle -> DamageVector
koukuCombinedDV = connectDV getKouku mempty calcKoukuDamageCombined

supportAirAttackDV :: Battle -> DamageVector
supportAirAttackDV = connectDV getSupportAirInfo mempty calcSupportAirAttackDamage

supportHouraiDV :: Battle -> DamageVector
supportHouraiDV = connectDV getSupportHouraiInfo mempty calcSupportHouraiDamage

kouku2DV :: Battle -> LR DamageVector
kouku2DV = connectDV getKouku2 memptyLR calcKoukuDamage

kouku2CombinedDV :: Battle -> DamageVector
kouku2CombinedDV = connectDV getKouku2 mempty calcKoukuDamageCombined

openingTaisenDV :: Battle -> LR DamageVector
openingTaisenDV = connectDV getOpeningTaisen memptyLR calcHougekiDamage

openingDV :: Battle -> LR DamageVector
openingDV = connectDV getOpeningAttack memptyLR calcRaigekiDamage

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

koukuDVAC :: Battle -> LR (LR DamageVector)
koukuDVAC = connectDV getKouku mt calcKoukuDamageAC

koukuDVBC :: Battle -> LR (LR DamageVector)
koukuDVBC = koukuDVAC


supportAirAttackDVAC :: Battle -> LR (LR DamageVector)
supportAirAttackDVAC = connectDV getSupportAirInfo mt calcSupportAirAttackDamageAC

supportHouraiDVAC :: Battle -> LR (LR DamageVector)
supportHouraiDVAC = connectDV getSupportHouraiInfo mt calcSupportHouraiDamageAC

kouku2DVAC :: Battle -> LR (LR DamageVector)
kouku2DVAC = connectDV getKouku2 mt calcKoukuDamageAC

kouku2DVBC :: Battle -> LR (LR DamageVector)
kouku2DVBC = kouku2DVAC

openingDVAC :: Battle -> LR (LR DamageVector)
openingDVAC = connectDV getOpeningAttack mt calcRaigekiDamageAC

openingTaisenDVAC :: Battle -> LR (LR DamageVector)
openingTaisenDVAC = connectDV getOpeningTaisen mt calcHougekiDamageAC

connectDV :: forall a b. (Battle -> Maybe a) -> b -> (a -> b) -> Battle -> b
connectDV getData z calc b = maybe z calc (getData b)

mt :: LR (LR DamageVector)
mt = { left: memptyLR, right: memptyLR }

fconcat :: Array (Battle -> LR DamageVector) -> Battle -> LR DamageVector
fconcat xs b = foldl lrAppend memptyLR ((\f -> f b) <$> xs)

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

fconcat2AC :: Array (Battle -> LR (LR DamageVector)) -> Battle -> LR (LR DamageVector)
fconcat2AC xs b = foldl auxAppend mt ((\f -> f b) <$> xs)
  where
    auxAppend l r =
        { left: lrAppend l.left r.left
        , right: lrAppend l.right r.right }
