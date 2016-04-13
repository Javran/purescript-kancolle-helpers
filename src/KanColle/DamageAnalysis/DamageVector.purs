module KanColle.DamageAnalysis.DamageVector
  ( DamageVector(..)
  , CombinedDamageVector(..)
  , FleetRole(..)
  , calcKoukuDamage
  , calcKoukuDamageCombined
  , calcHougekiDamage
  , calcRaigekiDamage
  , calcSupportAirAttackDamage
  , calcSupportHouraiDamage
  , toCombined
  ) where

import Prelude
import Math
import Data.Array
import qualified Data.Array.Unsafe as AU
import Data.Foldable
import Data.Int hiding (ceil,floor,round)
import Data.Maybe
import Data.Monoid
import Data.Monoid.Endo
import Data.Foreign

import KanColle.KCAPI.Battle

-- | `DamageVector` is an array of integers that a value `x :: Array Int`
-- |  satisfies the following properties
-- | * `length x == 13`
-- | * `head x == Just 0`
-- | * `all (>= 0) x`
newtype DamageVector = DV (Array Int)

instance damageVectorSemigroup :: Semigroup DamageVector where
  append (DV a) (DV b) = DV (zipWith (+) a b)

instance damageVectorMonoid :: Monoid DamageVector where
  mempty = DV (replicate 13 0)

instance damageVectorShow :: Show DamageVector where
  show (DV a) = "DV " <> show a

-- | normalize damage, there are cases where the number not turns out
-- | to be integers (0.1 for example), so we normalize the damage by
-- | taking floor of it
-- | NOTE: as an array of damages does not necessarily contain 13 elements
-- | let's not make the return type `DamageVector`
damageNormalize :: Array Number -> Array Int
damageNormalize = map normalize
  where
    normalize x | x >= 0.0 = fromMaybe 0 (fromNumber (floor x))
    normalize _ = 0

modifyDamage :: (Array Int -> Array Int)
             -> DamageVector -> DamageVector
modifyDamage f (DV s) = DV (f s)

-- | get `DamageVector` from raw `fDam` and `eDam` fields
fromFDamAndEDam :: forall a.
                { api_fdam :: Array Number
                , api_edam :: Array Number | a} -> DamageVector
fromFDamAndEDam v = DV ([0] <> damageNormalize (AU.tail v.api_fdam)
                            <> damageNormalize (AU.tail v.api_edam))

-- | calculate damage from kouku (aerial) stages
calcKoukuDamage :: Kouku -> DamageVector
calcKoukuDamage kk = fromFDamAndEDam kk.api_stage3

calcKoukuDamageCombined :: Kouku -> DamageVector
calcKoukuDamageCombined kk = fromFDam kk.api_stage3_combined
  where
    fromFDam v = DV ([0] <> damageNormalize (AU.tail v.api_fdam)
                         <> replicate 6 0)

-- | calculate damage from hougeki (shelling) stages
calcHougekiDamage :: Hougeki -> DamageVector
calcHougekiDamage h =
    -- the order of taking damage doesn't matter for the purpose
    -- of simulating final results, so the modifiers form a monoid of endofunctions
    -- we combine modifiers as the "simulator", feed an empty damage vector
    -- to it and get the simulated result back
    modifyDamage (runEndo modifiers) mempty
  where
    cAI :: Foreign -> Array Int
    cAI = unsafeFromForeign
    cAN :: Foreign -> Array Number
    cAN = unsafeFromForeign
    -- hougeki are represented like "events"
    -- so we need to "replay" events to calculate the damage.
    -- INVARIANT: length eventTargets == length eventDamages
    eventTargets :: Array Int
    eventTargets = concat (map cAI (AU.tail h.api_df_list))
    eventDamages :: Array Int
    eventDamages = damageNormalize (concat (map cAN (AU.tail h.api_damage)))

    modifiers = foldMap Endo (zipWith toDamageModifier eventTargets eventDamages)
    toDamageModifier :: Int -> Int -> Array Int -> Array Int
    toDamageModifier target damage arr =
        fromMaybe arr (modifyAt target (+ damage) arr)

-- | calculate damage from raigeki (torpedo) stages
calcRaigekiDamage :: Raigeki -> DamageVector
calcRaigekiDamage = fromFDamAndEDam

calcSupportAirAttackDamage :: SupportAirInfo -> DamageVector
calcSupportAirAttackDamage info = DV $
    [-1] <> replicate 6 0 <> damageNormalize (AU.tail info.api_stage3.api_edam)

calcSupportHouraiDamage :: SupportHouraiInfo -> DamageVector
calcSupportHouraiDamage info = DV $
    [-1] <> replicate 6 0 <> damageNormalize (AU.tail info.api_damage)

data FleetRole = FRMain | FREscort | FRSupport

newtype CombinedDamageVector = CDV
  { main    :: DamageVector
  , escort  :: DamageVector
  , support :: DamageVector }

instance combinedDamageVectorSemigroup :: Semigroup CombinedDamageVector where
  append (CDV a) (CDV b) = CDV
      { main:    a.main    <> b.main
      , escort:  a.escort  <> b.escort
      , support: a.support <> b.support
      }

instance combinedDamageVectorMonoid :: Monoid CombinedDamageVector where
  mempty = CDV { main: mempty, escort: mempty, support: mempty }

toCombined :: FleetRole -> DamageVector -> CombinedDamageVector
toCombined r dv = case r of
    FRMain    -> CDV { main: dv, escort: mempty, support: mempty }
    FREscort  -> CDV { main: mempty, escort: dv, support: mempty }
    FRSupport -> CDV { main: mempty, escort: mempty, support: dv }
