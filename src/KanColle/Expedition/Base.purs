module KanColle.Expedition.Base
  ( ResourceRows(..)
  , resourceRowsFill
  , resourceRowsLiftOp
  , mapResourceRows
  , allExpeditionIds
  ) where

-- basic expedition information
-- this module is intended to be imported by other expedition modules
-- so that we can solve the circular module dependency issue

import Prelude
import Data.Array
import Data.Monoid

allExpeditionIds :: Array Int
allExpeditionIds = (1..32) <> (35..40)

-- | `ResourceRows a` represents attributes of 4 resources.
-- | All of the attributes have to be of the same type, namely `a`.
type ResourceRows a =
  { fuel :: a
  , ammo :: a
  , steel :: a
  , bauxite :: a
  }

resourceRowsFill :: forall a. a -> ResourceRows a
resourceRowsFill v =
    { fuel: v
    , ammo: v
    , steel: v
    , bauxite: v
    }

resourceRowsLiftOp :: forall a b.
                      (a -> a -> b)
                   -> ResourceRows a
                   -> ResourceRows a
                   -> ResourceRows b
resourceRowsLiftOp app x y =
    { fuel: app x.fuel y.fuel
    , ammo: app x.ammo y.ammo
    , steel: app x.steel y.steel
    , bauxite: app x.bauxite y.bauxite
    }

mapResourceRows :: forall a b.
                   (a -> b)
                -> ResourceRows a
                -> ResourceRows b
mapResourceRows f r =
    { fuel: f r.fuel
    , ammo: f r.ammo
    , steel: f r.steel
    , bauxite: f r.bauxite
    }
