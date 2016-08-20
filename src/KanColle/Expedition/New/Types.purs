module KanColle.Expedition.New.Types 
  ( module KanColle.Expedition.New.SType
  , module KanColle.Expedition.New.Item
  , FleetCompo
  , MinFleetCompo
  ) where

import Data.Maybe
import KanColle.Expedition.New.SType
import KanColle.Expedition.New.Item

type FleetCompo = Array SType

-- Nothing: ship type not specified
-- Just <stype>: must be of ship type <stype>
type MinFleetCompo = Array (Maybe SType)
