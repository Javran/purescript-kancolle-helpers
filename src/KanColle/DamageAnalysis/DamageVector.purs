{- | This module implements `DamageVector`:
   | an array of `Damage`s that can be applied to one fleet of `Ship`s.
   | most functions prefixed with `calc` in this module
   | produces either `LR DamageVector` or a single `DamageVector`.
   | which can then be converted `NormalDamageVector` or `CombinedDamageVector`
   | depending on what the corresponding data is standing for.
 -}
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
  , calcLandBasedKoukuDamage

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
import Data.Unfoldable
import Control.Monad.Eff
import Control.Monad.ST as ST
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

-- | `DamageVector` is a 6-element array of `Damage`s
newtype DamageVector = DV (Array Damage)

-- | fetches the array inside `DamageVector`
getDV :: DamageVector -> Array Damage
getDV (DV v) = v

-- | creates `DamageVector` from arrays.
-- | array has to be of length 6
mkDV :: Array Damage -> DamageVector
mkDV xs = if check
    then DV xs
    else throwWith "mkDV: array size should be 6"
  where
    check = A.length xs == 6

instance semigroupDamageVector :: Semigroup DamageVector where
  append (DV a) (DV b) = DV (A.zipWith (<>) a b)

instance monoidDamageVector :: Monoid DamageVector where
  mempty = DV (replicate 6 mempty)

-- | `DamageVector` for normal battles
type NormalDamageVector = NormalBattle DamageVector

-- | `DamageVector` for battles involving comined fleets
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

-- (internal use only)
-- an "-1" is put in front of both api_fdam and api_edam
-- we first drop that element and then convert
-- integers intos Damages
convertFEDam :: Array Number -> Array Damage
convertFEDam = unsafeArrTail >>> map (normalizeDamage >>> mkDamage)

-- | calculate damage from kouku (aerial) stages
calcKoukuDamage :: Kouku -> LR DamageVector
calcKoukuDamage kk = fromFDamAndEDam kk.api_stage3

-- | calculate damage from kouku (aerial) stages (combined fleet)
-- | note that only escort fleet is taking damage. so we just need DamageVector
calcKoukuDamageCombined :: Kouku -> DamageVector
calcKoukuDamageCombined kk = DV (convertFEDam (kk.api_stage3_combined.api_fdam))

-- | calculate land based kouku damages. whose api_stage3 only have an api_edam field
-- | (because only enemies are taking damage)
calcLandBasedKoukuDamage :: Kouku -> DamageVector
calcLandBasedKoukuDamage lbkk = case maybeStage3 of
    Just stage3 -> DV (convertFEDam stage3.api_edam)
    Nothing -> mempty
  where
    maybeStage3 = getKoukuStage3Maybe lbkk

-- | calculate damage from raigeki (torpedo) stages
calcRaigekiDamage :: Raigeki -> LR DamageVector
calcRaigekiDamage = fromFDamAndEDam

-- | calculate damage from hougeki (shelling) stages
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
    eventTargets = map (cAI >>> toOne) (unsafeArrTail h.api_df_list)
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
    eventDamages = map (cAN >>> convert) (unsafeArrTail h.api_damage)
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
        arr <- STA.thaw (replicate 12 mempty)
        A.zipWithA (accumulateDamage arr) eventTargets eventDamages
        pure arr

-- | calculate damage from support airstrike stages.
-- | note that only enemy is taking damage so this results in
-- | a single `DamageVector`.
calcSupportAirAttackDamage :: SupportAirInfo -> DamageVector
calcSupportAirAttackDamage info = DV $ convertFEDam info.api_stage3.api_edam

-- | calculate damage from support shelling stages.
-- | note that only enemy is taking damage so this results in
-- | a single `DamageVector`.
calcSupportHouraiDamage :: SupportHouraiInfo -> DamageVector
calcSupportHouraiDamage info = DV $ convertFEDam info.api_damage

-- | ally fleet's role in this battle
data FleetRole
  = FRMain -- ally main vs. enemy main
  | FREscort -- ally escort vs. enemy main
  | FRSupport -- ally support exped vs. enemy main
  | FRLandBased -- ally LBAS vs. enemy main

-- | `toCombined role dv` converts a `LR DamageVector` whose left part
-- | is playing role `role` into `CombinedDamageVector`
toCombined :: FleetRole -> LR DamageVector -> CombinedDamageVector
toCombined r dv = case r of
    FRMain      -> { main: dv.left, escort: mempty, enemy: dv.right }
    FREscort    -> { main: mempty, escort: dv.left, enemy: dv.right }
    FRSupport   -> { main: mempty, escort: mempty, enemy: dv.right }
    FRLandBased -> { main: mempty, escort: mempty, enemy: dv.right }

-- | apply a single `DamageVector` on a single fleet
applyDamageVector :: DamageVector -> FleetInfo Ship -> FleetInfo Ship
applyDamageVector dv fleet = A.zipWith combine (getDV dv) fleet
  where
    combine :: Damage -> Maybe Ship -> Maybe Ship
    combine dmg ms = applyDamage dmg <$> ms

-- | apply `NormalDamageVector` on a normal fleet of ships (including enemy ships)
applyNormalDamageVector :: NormalDamageVector -> NormalFleetInfo Ship -> NormalFleetInfo Ship
applyNormalDamageVector ndv fleet =
    dupAsNormalBattle applyDamageVector
      `appNormalBattle` ndv
      `appNormalBattle` fleet

-- | apply `CombinedDamageVector` on a combined fleet of ships (including enemy ships)
applyCombinedDamageVector :: CombinedDamageVector -> CombinedFleetInfo Ship -> CombinedFleetInfo Ship
applyCombinedDamageVector ndv fleet =
    dupAsCombinedBattle applyDamageVector
      `appCombinedBattle` ndv
      `appCombinedBattle` fleet
