module KanColle.Expedition.New.Types 
  ( module KanColle.Expedition.New.SType
  , module KanColle.Expedition.New.Item
  , FleetCompo
  , MinFleetCompo
  , FleetMaxCost
  , Info
  , ItemInfo
  , MaxCost(..)
  , mkMC
  , CostModel
  ) where

import Prelude
import Data.Maybe
import KanColle.Expedition.New.SType
import KanColle.Expedition.New.Item
import Data.Map as M
import Data.Tuple
import Data.Traversable
import Data.Int

-- Nothing: ship type not specified
-- Just <stype>: must be of ship type <stype>
type MinFleetCompo = Array (Maybe SType)
type FleetCompo = Array SType
type FleetMaxCost = Array MaxCost

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

calcFleetMaxCost :: CostModel -> FleetCompo -> Maybe FleetMaxCost
calcFleetMaxCost cm fcAr = fold <$> sequence compos
  where
    fc :: M.Map SType Int
    fc = M.fromFoldableWith (+) (map (\x -> Tuple x 1) fcAr)
    compos = map (\(Tuple sty cnt) -> cm sty cnt) (M.toList fc)

calcActualCost :: MaxCost -> Info -> ActualCost
calcActualCost (MCost mc) info = ACost
    { fuel: floor (info.fuelCostPercent * toNumber mc.fuel)
    , ammo: floor (info.ammoCostPercent * toNumber mc.ammo)
    }

calcFleetActualCost :: FleetMaxCost -> Info -> FleetActualCost
calcFleetActualCost fmc info = FACost (foldl merge z fmc)
  where
    z = {fuel: 0, ammo: 0}
    merge acc mc = case calcActualCost mc info of
      (ACost actual) ->
        {fuel: acc.fuel+actual.fuel, ammo: acc.ammo+actual.ammo}

