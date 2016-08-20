module KanColle.Expedition.New.Types 
  ( module KanColle.Expedition.New.SType
  , module KanColle.Expedition.New.Item
  , FleetCompo
  , MinFleetCompo
  , Info
  , ItemInfo
  ) where

import Data.Maybe
import KanColle.Expedition.New.SType
import KanColle.Expedition.New.Item

type FleetCompo = Array SType

-- Nothing: ship type not specified
-- Just <stype>: must be of ship type <stype>
type MinFleetCompo = Array (Maybe SType)

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
