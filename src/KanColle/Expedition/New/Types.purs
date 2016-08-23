module KanColle.Expedition.New.Types 
  ( module KanColle.Expedition.New.SType
  , module KanColle.Expedition.New.Item
  , FleetCompo
  , MinFleetCompo
  , FleetMaxCost
  , Info
  , ItemInfo
  , MaxCost(..)
  , ActualCost(..)
  , FleetActualCost(..)
  , Resource(..)
  , FleetNetIncome(..)
  , mkMC
  , CostModel
  ) where

import Data.Maybe
import KanColle.Expedition.New.SType
import KanColle.Expedition.Base
import KanColle.Expedition.New.Item

-- Nothing: ship type not specified
-- Just <stype>: must be of ship type <stype>
type MinFleetCompo = Array (Maybe SType)
type FleetCompo = Array SType
type FleetMaxCost = Array MaxCost

newtype Resource = Rsc (ResourceRows Int)
newtype FleetNetIncome = FNI (ResourceRows Int)

type Info =
  { id :: Int
  , timeInMin :: Int
  , fuelCostPercent :: Number
  , ammoCostPercent :: Number
  , item1 :: Maybe ItemInfo
  , item2 :: Maybe ItemInfo
  }

type ItemInfo =
  { item :: Item
  , maxCount :: Int
  }

newtype MaxCost = MCost
  { fuel :: Int
  , ammo :: Int
  }
  
newtype ActualCost = ACost
  { fuel :: Int
  , ammo :: Int
  }
  
newtype FleetActualCost = FACost
  { fuel :: Int
  , ammo :: Int
  }

mkMC :: Int -> Int -> MaxCost
mkMC fuel ammo = MCost {fuel: fuel, ammo: ammo}

-- | `CostModel` calculates the total maximum cost
-- | given a ship type and how many ships are present.
-- | This cost model is designed for just one fleet so
-- | the input number should only be one of `[0,1,2,3,4,5,6]`.
-- | Note that for a cost model `f`, it's not guaranteed
-- | that `f stype a + f stype b` and `f stype (a+b)` give
-- | the same answer.
type CostModel = SType -> Int -> Maybe (Array MaxCost)
