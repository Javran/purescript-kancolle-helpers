module KanColle.DamageAnalysis.Damage
  ( DameCon(..)
  , Ship
  , Damage
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

data DameCon
  = RepairTeam
  | RepairGoddess
  
derive instance eqDameCon :: Eq DameCon

instance showDameCon :: Show DameCon where
  show c = case c of
      RepairTeam -> "RepairTeam"
      RepairGoddess -> "RepairGoddess"

type Ship =
  { hp :: Int
  , fullHp :: Int
  , sunk :: Boolean
  , dameCon :: Maybe DameCon
  }

-- a Damage modifies a ship's state properly
newtype Damage = Damage (Ship -> Ship)

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

-- apply DameCon to a Ship, regardless of ship's own DameCon setting
applyDameCon :: DameCon -> Ship -> Ship
applyDameCon dc s = case dc of
    RepairTeam -> s1 {hp = Int.floor (0.2 * Int.toNumber s.fullHp)}
    RepairGoddess -> s1 {hp = s.fullHp}
  where
    s1 = s {sunk = false, dameCon = Nothing}

-- when a Ship is sinking, apply DameCon to a Ship if any.
tryDameCon :: Ship -> Ship
tryDameCon s = if s.sunk
    then -- only attempt applying damecon if the ship is sinking
      maybe s (\dc -> applyDameCon dc s) s.dameCon
    else s

-- apply a damage number to a ship
applyDamageNum :: Int -> Ship -> Ship
applyDamageNum v s =
    let newHp = s.hp - v
    in s {hp = newHp, sunk = newHp <= 0}

-- create "Damage"
mkDamage :: Int -> Damage
mkDamage 0 = Damage id
mkDamage v = Damage (applyDamageNum v >>> tryDameCon)

-- create new Damage by applying two Damages in order
mergeDamage :: Damage -> Damage -> Damage
mergeDamage (Damage d1) (Damage d2) = Damage (d1 >>> d2)

-- add damage value to an existing Damage
addDamage :: Damage -> Int -> Damage
addDamage dmg v = dmg `mergeDamage` mkDamage v

-- apply Damage to a Ship, DameCon will be used if applicable.
applyDamage :: Damage -> Ship -> Ship
applyDamage (Damage f) = f

instance semigroupDamage :: Semigroup Damage where
   append = mergeDamage

instance monoidDamage :: Monoid Damage where
   mempty = Damage id
