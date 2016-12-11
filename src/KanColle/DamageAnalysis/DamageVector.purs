{- | This module implements `DamageVector`:
   | an array of `Damage`s that can be applied to one fleet of `Ship`s.
   | most functions prefixed with `calc` in this module
   | produces either `LR DamageVector` or a single `DamageVector`.
   | which can then be converted `NormalDamageVector` or `CombinedDamageVector`
   | depending on what the corresponding data is standing for.
 -}
module KanColle.DamageAnalysis.DamageVector
  ( DamageVector, getDV, mkDV

  , NormalDamageVector
  , GCombinedDamageVector
  , CombinedDamageVector, CombinedDamageVectorAC
  , FleetRole(..)

  , calcKoukuDamage
  , calcKoukuDamageAC
  , calcKoukuDamageCombined
  , calcSupportAirAttackDamage
  , calcSupportAirAttackDamageAC
  , calcSupportHouraiDamage
  , calcSupportHouraiDamageAC
  , calcHougekiDamage
  , calcRaigekiDamage

  , calcRaigekiDamageAC
  , calcHougekiDamageAC

  , calcLandBasedKoukuDamage

  , toGCombined
  , toCombined
  , toCombinedAC
  , toCombinedBC  

  , applyDamageVector
  , applyNormalDamageVector
  , applyCombinedDamageVector
  ) where

import Prelude
import Data.Monoid
import Data.Int as Int
import Data.Array as A
import Data.Array.ST as STA
import Data.Unfoldable hiding (fromMaybe)
import Control.Monad.Eff
import Control.Monad.ST as ST
import KanColle.Util
import KanColle.DamageAnalysis.Types
import KanColle.DamageAnalysis.Damage

import Data.Tuple
import Data.Traversable
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
type GCombinedDamageVector = GCombinedBattle DamageVector
type CombinedDamageVector = CombinedBattle DamageVector
type CombinedDamageVectorAC = CombinedBattleAC DamageVector

debugShowDV :: DamageVector -> String
debugShowDV (DV xs) = joinWith "," (map (show <<< damageToInt) xs)

-- | get `DamageVector` from raw `fDam` and `eDam` fields
fromFDamAndEDam :: KoukuStage3 -> LR DamageVector
fromFDamAndEDam ks3 =
    { left: fromMaybe mempty ((convertFEDam >>> mkDV) <$> fDam)
    , right: fromMaybe mempty ((convertFEDam >>> mkDV) <$> eDam) }
  where
    fDam = getKoukuStage3FDam ks3
    eDam = getKoukuStage3EDam ks3

-- L: ally fleet, LL: ally main, LR: ally escort
-- R: enemy fleet: RL: enemy main, RR: enemy escort
fromFDamAndEDamAC :: forall a.
                  { api_fdam :: KArray Number
                  , api_edam :: KArray Number | a} -> LR (LR DamageVector)
fromFDamAndEDamAC v = (lrMap >>> lrMap) mkDV
    { left: allyDams
    , right: enemyDams }
  where
    longFDam = convertFEDam v.api_fdam
    allyDams = fleetSplit false longFDam

    longEDam = convertFEDam v.api_edam
    enemyDams = fleetSplit false longEDam

-- (internal use only)
-- an "-1" is put in front of both api_fdam and api_edam
-- we first drop that element and then convert
-- integers intos Damages
convertFEDam :: KArray Number -> Array Damage
convertFEDam = fromKArray >>> map (normalizeDamage >>> mkDamage)

-- | calculate damage from kouku (aerial) stages
calcKoukuDamage :: Kouku -> LR DamageVector
calcKoukuDamage kk = fromFDamAndEDam kk.api_stage3

-- | calculate damage from ally vs. enemy main fleet kouku stages (for Abyssal Combined fleet)
calcKoukuDamageACEMain :: forall a . {api_stage3 :: KoukuStage3 | a} -> LR DamageVector
calcKoukuDamageACEMain kk = fromFDamAndEDam kk.api_stage3

-- | calculate damage from ally vs. enemy escort fleet kouku stages (for Abyssal Combined fleet)
calcKoukuDamageACEEscort :: forall a. {api_stage3_combined :: KoukuStage3 | a} -> LR DamageVector
calcKoukuDamageACEEscort kk = fromFDamAndEDam kk.api_stage3_combined

calcKoukuDamageAC :: forall a.
                  { api_stage3 :: KoukuStage3
                  , api_stage3_combined :: KoukuStage3
                  | a} -> LR (LR DamageVector)
calcKoukuDamageAC kk =
    { left:
      { left: resultCMain.left
      , right: resultCEscort.left }
    , right:
      { left: resultCMain.right
      , right: resultCEscort.right } }
  where
    resultCMain = calcKoukuDamageACEMain kk
    resultCEscort = calcKoukuDamageACEEscort kk

-- | calculate damage from kouku (aerial) stages (combined fleet)
-- | note that only escort fleet is taking damage. so we just need DamageVector
calcKoukuDamageCombined :: Kouku -> DamageVector
calcKoukuDamageCombined kk = DV (convertFEDam (kk.api_stage3_combined.api_fdam))

-- | calculate land based kouku damages. whose api_stage3 only have an api_edam field
-- | (because only enemies are taking damage)
calcLandBasedKoukuDamage :: Kouku -> DamageVector
calcLandBasedKoukuDamage lbkk = case getKoukuStage3 lbkk of
    Just stage3 -> DV (convertFEDam stage3.api_edam)
    Nothing -> mempty

-- | calculate damage from raigeki (torpedo) stages
calcRaigekiDamage :: Raigeki -> LR DamageVector
calcRaigekiDamage = fromFDamAndEDam

calcRaigekiDamageAC :: Raigeki -> LR (LR DamageVector)
calcRaigekiDamageAC = fromFDamAndEDamAC

-- | calculate damage from hougeki (shelling) stages
calcHougekiDamage :: Hougeki -> LR DamageVector
calcHougekiDamage h =
    if lengthCheck
      then let resultArr = simulateHougeki 12 (A.zipWith Tuple eventTargets eventDamages)
           in lrMap DV (fleetSplit false resultArr)
      else throwWith "invalid: api_df_list and api_damage length mismatch"
  where
    lengthCheck = A.length eventTargets == A.length eventDamages

    eventTargets :: Array Int
    eventTargets = map toOne (fromKArray h.api_df_list)
      where
        -- merge targets like [a,a] into a-1 (so that it's zero-indexed)
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
    eventDamages = map convert (fromKArray h.api_damage)
      where
        convert :: Array Number -> Damage
        convert xs = mkDamage totalDmg
          where
            -- there are double attacks in the damage list
            -- because damecon triggers *AFTER* the attack actions are completed
            -- we need to sum damage values before turning it into real Damage
            totalDmg = sum (map normalizeDamage xs :: Array Int)

-- I could do (forall r. (Int -> Damage -> r) -> r) in place of (Tuple Int Damage)
-- but I can't persuade PS that is correct.
simulateHougeki :: forall f. Traversable f => Int -> f (Tuple Int Damage) -> Array Damage
simulateHougeki len actions = runPure (STA.runSTArray (resultDmgs))
  where
    accumulateDamage :: forall h r.
                        STA.STArray h Damage -- array reference
                     -> Int -> Damage -- target index and corresponding damage
                     -> Eff (st :: ST.ST h | r) Unit
    accumulateDamage arr targetInd damage = do
        dmg <- peekSTArrayUnsafe arr targetInd
        pokeSTArrayUnsafe arr targetInd (dmg <> damage :: Damage)
        pure unit

    resultDmgs :: forall h r . Eff (st :: ST.ST h | r) (STA.STArray h Damage)
    resultDmgs = do
        arr <- STA.thaw (replicate len mempty)
        traverse (uncurry $ accumulateDamage arr) actions
        pure arr

calcHougekiDamageAC :: Hougeki -> LR (LR DamageVector)
calcHougekiDamageAC h =
    if lengthCheck
      then let resultArr = simulateHougeki 24 (A.zipWith Tuple eventTargets eventDamages)
           in { left:
                { left: DV (A.slice 0 6 resultArr)
                , right: DV (A.slice 6 12 resultArr) }
              , right:
                { left: DV (A.slice 12 18 resultArr)
                , right: DV (A.slice 18 24 resultArr) }
              }
      else throwWith "invalid: api_df_list / api_damage / api_at_eflag length mismatch"
  where
    dfList = fromKArray h.api_df_list
    atEFlag :: Array Int
    atEFlag = fromKArray h.api_at_eflag
    damageList = fromKArray h.api_damage

    lengthCheck = A.length dfList == A.length atEFlag
               && A.length dfList == A.length damageList

    eventTargetsRaw :: Array Int
    eventTargetsRaw = map toOne (fromKArray h.api_df_list)
      where
        -- merge targets like [a,a] into a-1 (so that it's zero-indexed)
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

    eventTargets :: Array Int
    eventTargets = A.zipWith updateTarget eventTargetsRaw atEFlag
      where
        updateTarget rawTarget eFlag = case eFlag of
            -- ally attacks in this turn, so the target is an enemy
            0 -> rawTarget + 12
            -- enemy attacks in this turn, so the target is an ally
            -- nothing to be doing in this case
            1 -> rawTarget
            _ -> throwWith "invalid api_at_eflag element"

    eventDamages :: Array Damage
    eventDamages = map convert (fromKArray h.api_damage)
      where
        convert :: Array Number -> Damage
        convert xs = mkDamage totalDmg
          where
            -- there are double attacks in the damage list
            -- because damecon triggers *AFTER* the attack actions are completed
            -- we need to sum damage values before turning it into real Damage
            totalDmg = sum (map normalizeDamage xs :: Array Int)

-- | calculate damage from support airstrike stages.
-- | note that only enemy is taking damage so this results in
-- | a single `DamageVector`.
calcSupportAirAttackDamage :: SupportAirInfo -> DamageVector
calcSupportAirAttackDamage info = DV $ convertFEDam info.api_stage3.api_edam

calcSupportAirAttackDamageAC :: SupportAirInfo -> LR (LR DamageVector)
calcSupportAirAttackDamageAC = calcKoukuDamageAC

-- | calculate damage from support shelling stages.
-- | note that only enemy is taking damage so this results in
-- | a single `DamageVector`.
calcSupportHouraiDamage :: SupportHouraiInfo -> DamageVector
calcSupportHouraiDamage info = DV $ convertFEDam info.api_damage

-- TODO: to be verified, for now I just guess it's an array of raw length 1+12
calcSupportHouraiDamageAC :: SupportHouraiInfo -> LR (LR DamageVector)
calcSupportHouraiDamageAC info = { left: memptyLR, right: lrMap mkDV converted }
  where
    converted = fleetSplit false (convertFEDam info.api_damage)

-- | ally fleet's role in this battle
data FleetRole
  = FRMain -- ally main vs. enemy main
  | FREscort -- ally escort vs. enemy main
  | FRSupport -- ally support exped vs. enemy main
  | FRLandBased -- ally LBAS vs. enemy main

-- | `toCombined role dv` converts a `LR DamageVector` whose left part
-- | is playing role `role` into `CombinedDamageVector`
toGCombined :: FleetRole -> LR DamageVector -> GCombinedDamageVector
toGCombined r dv = case r of
    FRMain      ->
        { allyMain: dv.left, allyEscort: mempty
        , enemyMain: dv.right, enemyEscort: mempty }
    FREscort    ->
        { allyMain: mempty, allyEscort: dv.left
        , enemyMain: dv.right, enemyEscort: mempty }
    FRSupport   ->
        { allyMain: mempty, allyEscort: mempty
        , enemyMain: dv.right, enemyEscort: mempty }
    FRLandBased ->
        { allyMain: mempty, allyEscort: mempty
        , enemyMain: dv.right, enemyEscort: mempty }

toCombined :: FleetRole -> LR DamageVector -> CombinedDamageVector
toCombined r dv = toCombinedBattle (toGCombined r dv)

-- TODO: add check on dv.left.right, which should be all 0
toCombinedAC :: LR (LR DamageVector) -> CombinedDamageVectorAC
toCombinedAC dv =
    { main: dv.left.left
    , enemyMain: dv.right.left
    , enemyEscort: dv.right.right
    }
    

toCombinedBC :: LR (LR DamageVector) -> GCombinedDamageVector
toCombinedBC dv =
   { allyMain: dv.left.left
   , allyEscort: dv.left.right
   , enemyMain: dv.right.left
   , enemyEscort: dv.right.right
   }
   
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
