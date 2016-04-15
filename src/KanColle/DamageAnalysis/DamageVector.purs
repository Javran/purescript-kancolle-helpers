module KanColle.DamageAnalysis.DamageVector
  ( DamageVector, getDV, mkDV

  , NormalDamageVector, CombinedDamageVector
  , FleetRole(..)

  , calcKoukuDamage
  , calcKoukuDamageCombined
  , calcSupportAirAttackDamage
  , calcSupportHouraiDamage
  , calcHougekiDamage
  , calcRaigekiDamage

  , toCombined
  
  , applyDamageVector
  , applyNormalDamageVector
  , applyCombinedDamageVector
  ) where

import Prelude
import Data.Monoid
import Data.Int as Int
import Data.Array as A
import Data.Array.ST as STA
import Control.Monad.Eff
import Control.Monad.ST as ST
import Data.Array.Unsafe as AU
import KanColle.Util
import KanColle.DamageAnalysis.Types
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
newtype DamageVector = DV (Array Damage)

getDV :: DamageVector -> Array Damage
getDV (DV v) = v

mkDV :: Array Damage -> DamageVector
mkDV xs = if check 
    then DV xs
    else throwWith "mkDV: array size should be 6"
  where
    check = A.length xs == 6

instance semigroupDamageVector :: Semigroup DamageVector where
  append (DV a) (DV b) = DV (A.zipWith (<>) a b)

instance monoidDamageVector :: Monoid DamageVector where
  mempty = DV (A.replicate 6 mempty)

type NormalDamageVector = NormalBattle DamageVector
type CombinedDamageVector = CombinedBattle DamageVector

debugShowDV :: DamageVector -> String
debugShowDV (DV xs) = joinWith "," (map (show <<< damageToInt) xs)

-- | get `DamageVector` from raw `fDam` and `eDam` fields
fromFDamAndEDam :: forall a.
                { api_fdam :: Array Number
                , api_edam :: Array Number | a} -> LR DamageVector
fromFDamAndEDam v =
    { left: DV (convertFEDam v.api_fdam)
    , right: DV (convertFEDam v.api_edam) }

-- (internal)
-- an "-1" is put in front of both api_fdam and api_edam
-- we first drop that element and then convert
-- integers intos Damages
convertFEDam :: Array Number -> Array Damage
convertFEDam = AU.tail >>> map (normalizeDamage >>> mkDamage)

-- | calculate damage from kouku (aerial) stages
calcKoukuDamage :: Kouku -> LR DamageVector
calcKoukuDamage kk = fromFDamAndEDam kk.api_stage3

-- | calculate damage from kouku (aerial) stages (combined fleet)
-- | note that only escort fleet is taking damage. so we just need DamageVector
calcKoukuDamageCombined :: Kouku -> DamageVector
calcKoukuDamageCombined kk = DV (convertFEDam (kk.api_stage3_combined.api_fdam))

-- | calculate damage from raigeki (torpedo) stages
calcRaigekiDamage :: Raigeki -> LR DamageVector
calcRaigekiDamage = fromFDamAndEDam

calcHougekiDamage :: Hougeki -> LR DamageVector
calcHougekiDamage h =
    if lengthCheck
      then let resultArr = runPure (STA.runSTArray resultDV)
           in lrMap DV (fleetSplit false resultArr)
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
calcSupportAirAttackDamage :: SupportAirInfo -> DamageVector
calcSupportAirAttackDamage info = DV $ convertFEDam info.api_stage3.api_edam

-- only on enemy
calcSupportHouraiDamage :: SupportHouraiInfo -> DamageVector
calcSupportHouraiDamage info = DV $ convertFEDam info.api_damage

data FleetRole = FRMain | FREscort | FRSupport

toCombined :: FleetRole -> LR DamageVector -> CombinedDamageVector
toCombined r dv = case r of
    FRMain    -> { main: dv.left, escort: mempty, enemy: dv.right }
    FREscort  -> { main: mempty, escort: dv.left, enemy: dv.right }
    FRSupport -> { main: mempty, escort: mempty, enemy: dv.right }

applyDamageVector :: DamageVector -> FleetInfo Ship -> FleetInfo Ship
applyDamageVector dv fleet = A.zipWith combine (getDV dv) fleet
  where
    combine :: Damage -> Maybe Ship -> Maybe Ship
    combine dmg ms = applyDamage dmg <$> ms

applyNormalDamageVector :: NormalDamageVector -> NormalFleetInfo Ship -> NormalFleetInfo Ship
applyNormalDamageVector ndv fleet =
    dupAsNormalBattle applyDamageVector
      `appNormalBattle` ndv 
      `appNormalBattle` fleet
      
applyCombinedDamageVector :: CombinedDamageVector -> CombinedFleetInfo Ship -> CombinedFleetInfo Ship
applyCombinedDamageVector ndv fleet =
    dupAsCombinedBattle applyDamageVector
      `appCombinedBattle` ndv 
      `appCombinedBattle` fleet
