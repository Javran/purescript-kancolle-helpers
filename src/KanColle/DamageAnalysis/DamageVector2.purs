module KanColle.DamageAnalysis.DamageVector2
where

import Prelude
import Data.Monoid
import Data.Int as Int
import Data.Array as A
import Data.Array.ST as STA
import Control.Monad.Eff
import Control.Monad.ST as ST
import Data.Array.Unsafe as AU
import KanColle.Util
import KanColle.DamageAnalysis.Damage

import Data.Foreign
import Data.Foldable
import KanColle.KCAPI.Battle
import Data.Maybe
import Data.String (joinWith)
-- when flagship protection happens
-- and extra 0.1 will be added to that value
-- for our purpose we don't need to deal with that
-- so all damages can be normalized as Ints
normalizeDamage :: Number -> Int
normalizeDamage x
  | x >= 0.0 = Int.floor x
  | x == (-1.0) = 0
  | otherwise =
    let warning = "invalid damage number: " <> show x
    in traceWarn warning (\_ -> 0)

-- | `DamageVector` is an array of Damages
-- |  satisfies the following properties
-- | * `length x == 6`
newtype DamageVector2 = DV2 (Array Damage)

instance semigroupDamageVector2 :: Semigroup DamageVector2 where
  append (DV2 a) (DV2 b) = DV2 (A.zipWith (<>) a b)

instance monoidDamageVector2 :: Monoid DamageVector2 where
  mempty = DV2 (A.replicate 6 mempty)

type NormalBattle a =
  { main :: a, enemy :: a }

type CombinedBattle a =
  { main :: a, escort :: a, enemy :: a}
 
-- pure
dupAsNormalBattle :: forall a. a -> NormalBattle a
dupAsNormalBattle v = { main: v, enemy: v }

-- applicative
appNormalBattle :: forall a b. NormalBattle (a -> b) -> NormalBattle a -> NormalBattle b
appNormalBattle f a = { main: f.main a.main, enemy: f.enemy a.enemy }

-- pure
dupAsCombinedBattle :: forall a. a -> CombinedBattle a
dupAsCombinedBattle v = { main: v, escort: v, enemy: v }

-- applicative
appCombinedBattle :: forall a b. CombinedBattle (a -> b) -> CombinedBattle a -> CombinedBattle b
appCombinedBattle f a = { main: f.main a.main
                        , escort: f.escort a.escort
                        , enemy: f.enemy a.enemy }

normalBattleMap :: forall a b. (a -> b) -> NormalBattle a -> NormalBattle b
normalBattleMap f x = { main: f x.main, enemy: f x.enemy }

combinedBattleMap :: forall a b. (a -> b) -> CombinedBattle a -> CombinedBattle b
combinedBattleMap f x = { main: f x.main, escort: f x.escort, enemy: f x.enemy }

type NormalDamageVector = NormalBattle DamageVector2
type CombinedDamageVector = CombinedBattle DamageVector2

debugShowDV :: DamageVector2 -> String
debugShowDV (DV2 xs) = joinWith "," (map (show <<< damageToInt) xs)

-- | get `DamageVector2` from raw `fDam` and `eDam` fields
fromFDamAndEDam :: forall a.
                { api_fdam :: Array Number
                , api_edam :: Array Number | a} -> LR DamageVector2
fromFDamAndEDam v =
    { left: DV2 (convertFEDam v.api_fdam)
    , right: DV2 (convertFEDam v.api_edam) }

-- (internal)
-- an "-1" is put in front of both api_fdam and api_edam
-- we first drop that element and then convert
-- integers intos Damages
convertFEDam :: Array Number -> Array Damage
convertFEDam = AU.tail >>> map (normalizeDamage >>> mkDamage)

-- | calculate damage from kouku (aerial) stages
calcKoukuDamage :: Kouku -> LR DamageVector2
calcKoukuDamage kk = fromFDamAndEDam kk.api_stage3

-- | calculate damage from kouku (aerial) stages (combined fleet)
-- | note that only escort fleet is taking damage. so we just need DamageVector2
calcKoukuDamageCombined :: Kouku -> DamageVector2
calcKoukuDamageCombined kk = DV2 (convertFEDam (kk.api_stage3_combined.api_fdam))

-- | calculate damage from raigeki (torpedo) stages
calcRaigekiDamage :: Raigeki -> LR DamageVector2
calcRaigekiDamage = fromFDamAndEDam

calcHougekiDamage :: Hougeki -> LR DamageVector2
calcHougekiDamage h =
    if lengthCheck
      then let resultArr = runPure (STA.runSTArray resultDV)
           in lrMap DV2 (fleetSplit false resultArr)
      else throwWith "invalid: api_df_list and api_damage length mismatch"
  where
    cAI :: Foreign -> Array Int
    cAI = unsafeFromForeign
    cAN :: Foreign -> Array Number
    cAN = unsafeFromForeign

    lengthCheck = A.length eventTargets == A.length eventDamages

    eventTargets :: Array Int
    eventTargets = map (cAI >>> toOne) (AU.tail h.api_df_list)
      where
        -- merge targets like [a,a] into a-1
        -- there are 2 checks:
        --   elements of api_df_list should:
        --   * be non-empty
        --   * all elements of it should be the same
        -- the reason for minus 1 in the end is to make it zero-based
        toOne :: Array Int -> Int
        toOne xs = case A.uncons xs of
          Just {head: y, tail: ys} ->
            if all (\v -> v == y) ys || all (\v -> v == -1) ys
              then y - 1
              else throwWith "invalid: elements are different in api_df_list"
          Nothing -> throwWith "invalid: empty api_df_list element"

    eventDamages :: Array Damage
    eventDamages = map (cAN >>> convert) (AU.tail h.api_damage)
      where
        convert :: Array Number -> Damage
        convert xs = mkDamage totalDmg
          where
            -- there are double attacks in the damage list
            -- because damecon triggers *AFTER* the attack actions are completed
            -- we need to sum damage values before turning it into real Damage
            totalDmg = sum (map normalizeDamage xs :: Array Int)

    accumulateDamage :: forall h r.
                        STA.STArray h Damage -- array reference
                     -> Int -> Damage -- target index and corresponding damage
                     -> Eff (st :: ST.ST h | r) Unit
    accumulateDamage arr targetInd damage = do
        dmg <- peekSTArrayUnsafe arr targetInd
        pokeSTArrayUnsafe arr targetInd (dmg <> damage :: Damage)
        pure unit
    resultDV :: forall h r . Eff (st :: ST.ST h | r) (STA.STArray h Damage)
    resultDV = do
        arr <- STA.thaw (A.replicate 12 mempty)
        A.zipWithA (accumulateDamage arr) eventTargets eventDamages
        pure arr

-- only on enemy
calcSupportAirAttackDamage :: SupportAirInfo -> DamageVector2
calcSupportAirAttackDamage info = DV2 $ convertFEDam info.api_stage3.api_edam

-- only on enemy
calcSupportHouraiDamage :: SupportHouraiInfo -> DamageVector2
calcSupportHouraiDamage info = DV2 $ convertFEDam info.api_damage

data FleetRole = FRMain | FREscort | FRSupport

toCombined :: FleetRole -> LR DamageVector2 -> CombinedDamageVector
toCombined r dv = case r of
    FRMain    -> { main: dv.left, escort: mempty, enemy: dv.right }
    FREscort  -> { main: mempty, escort: dv.left, enemy: dv.right }
    FRSupport -> { main: mempty, escort: mempty, enemy: dv.right }

{-
-- current format:
-- * main: main vs enemy
-- * escort: escort vs enemy
-- * support: support vs enemy
newtype CombinedDamageVector2 = CDV
  { main    :: DamageVector2
  , escort  :: DamageVector2
  , support :: DamageVector2 }

instance combinedDamageVectorSemigroup :: Semigroup CombinedDamageVector2 where
  append (CDV a) (CDV b) = CDV
      { main:    a.main    <> b.main
      , escort:  a.escort  <> b.escort
      , support: a.support <> b.support
      }

instance combinedDamageVectorMonoid :: Monoid CombinedDamageVector2 where
  mempty = CDV { main: mempty, escort: mempty, support: mempty }

toCombined :: FleetRole -> DamageVector2 -> CombinedDamageVector2
toCombined r dv = case r of
    FRMain    -> CDV { main: dv, escort: mempty, support: mempty }
    FREscort  -> CDV { main: mempty, escort: dv, support: mempty }
    FRSupport -> CDV { main: mempty, escort: mempty, support: dv }
-}
