module KanColle.Expedition.New.NetIncome where

import Prelude
import KanColle.Expedition.New.Types

calcFleetNetIncome :: Resource -> FleetActualCost -> FleetNetIncome
calcFleetNetIncome (Rsc r) (FACost c) = FNI
    { fuel: r.fuel - c.fuel
    , ammo: r.ammo - c.ammo
    , steel: r.steel
    , bauxite: r.bauxite
    }
