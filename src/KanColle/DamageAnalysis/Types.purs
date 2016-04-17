module KanColle.DamageAnalysis.Types
  ( DameCon(..)
  , Ship
  , mkShip
  , ShipResult
  , getShipResult

  , NormalBattle, CombinedBattle
  , FleetInfo
  , NormalFleetInfo, CombinedFleetInfo

  , dupAsNormalBattle
  , appNormalBattle
  , mapNormalBattle
  
  , dupAsCombinedBattle
  , appCombinedBattle
  , mapCombinedBattle
  
  , lrToNormal

  ) where

import Prelude
import Data.Maybe
import Data.Array
import KanColle.Util

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

mkShip :: Int -> Int -> Maybe DameCon -> Ship
mkShip hp fullHp dc =
  { fullHp: fullHp
  , hp: hp
  , sunk: hp <= 0
  , dameCon: dc
  }

type NormalBattle a =
  { main :: a, enemy :: a }

type CombinedBattle a =
  { main :: a, escort :: a, enemy :: a}

lrToNormal :: forall a. LR a -> NormalBattle a
lrToNormal x = { main: x.left, enemy: x.right }

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

mapNormalBattle :: forall a b. (a -> b) -> NormalBattle a -> NormalBattle b
mapNormalBattle f x = { main: f x.main, enemy: f x.enemy }

mapCombinedBattle :: forall a b. (a -> b) -> CombinedBattle a -> CombinedBattle b
mapCombinedBattle f x = { main: f x.main, escort: f x.escort, enemy: f x.enemy }

type ShipResult =
  { hp :: Int
  , sunk :: Boolean
  , dameConConsumed :: Boolean
  }

getShipResult :: Ship -> Ship -> ShipResult
getShipResult sBefore sAfter =
  { hp: sAfter.hp
  , sunk: sAfter.hp <= 0
  , dameConConsumed: 
      isJust sBefore.dameCon && isNothing sAfter.dameCon
  }
  
-- invariant: the length is always 6
type FleetInfo a = Array (Maybe a)
type NormalFleetInfo a = NormalBattle (FleetInfo a)
type CombinedFleetInfo a = CombinedBattle (FleetInfo a)
