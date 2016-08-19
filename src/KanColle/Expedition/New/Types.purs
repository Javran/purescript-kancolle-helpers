module KanColle.Expedition.New.Types 
  ( module KanColle.Expedition.New.SType
  , FleetCompo
  , MinFleetCompo
  ) where

import Data.Maybe
import KanColle.Expedition.New.SType

type FleetCompo = Array SType

-- Nothing: ship type not specified
-- Just <stype>: must be of ship type <stype>
type MinFleetCompo = Array (Maybe SType)
