{- | This module implements `Damage`:
   | a value that can be applied to a `Ship`
   | and `DameCon` is automatically consumed in the process if necessary.
 -}
module KanColle.DamageAnalysis.Damage
  ( Damage
  , mkDamage
  , mergeDamage
  , addDamage
  , applyDamage
  , damageToInt
  )
where

import Prelude
import Data.Maybe
import Data.Monoid
import Data.Int as Int
import KanColle.DamageAnalysis.Types

-- | a `Damage` represents a damage value to be applied
-- | to a `Ship`
newtype Damage = Damage (Ship -> Ship)

-- | converts `Damage` into the damage value
damageToInt :: Damage -> Int
damageToInt dmg = dummyShip.hp - afterShip.hp
  where
    dummyShip =
      { hp: 10000
      , fullHp: 10000
      , sunk: false
      , dameCon: Nothing
      }

    afterShip = applyDamage dmg dummyShip

-- | apply DameCon to a `Ship`, regardless of ship's own DameCon setting
applyDameCon :: DameCon -> Ship -> Ship
applyDameCon dc s = case dc of
    RepairTeam -> s1 {hp = Int.floor (0.2 * Int.toNumber s.fullHp)}
    RepairGoddess -> s1 {hp = s.fullHp}
  where
    s1 = s {sunk = false, dameCon = Nothing}

-- | when a `Ship` is sinking, apply DameCon if applicable
tryDameCon :: Ship -> Ship
tryDameCon s = if s.sunk
    then -- only attempt applying damecon if the ship is sinking
      maybe s (\dc -> applyDameCon dc s) s.dameCon
    else s

-- | apply a damage number to a ship
applyDamageNum :: Int -> Ship -> Ship
applyDamageNum v s =
    let newHp = s.hp - v
    in s {hp = newHp, sunk = newHp <= 0}

-- | create `Damage` from damage value
mkDamage :: Int -> Damage
mkDamage 0 = Damage id
mkDamage v = Damage (applyDamageNum v >>> tryDameCon)

-- | create new `Damage` by applying two `Damage`s in order
mergeDamage :: Damage -> Damage -> Damage
mergeDamage (Damage d1) (Damage d2) = Damage (d1 >>> d2)

-- | add damage value to an existing `Damage`
addDamage :: Damage -> Int -> Damage
addDamage dmg v = dmg `mergeDamage` mkDamage v

-- | apply `Damage` to a `Ship`, DameCon will be used if applicable.
applyDamage :: Damage -> Ship -> Ship
applyDamage (Damage f) = f

instance semigroupDamage :: Semigroup Damage where
   append = mergeDamage

instance monoidDamage :: Monoid Damage where
   mempty = Damage id
