module KanColle.Expedition.New.Scorer where

import Prelude
import KanColle.Expedition.Base
import KanColle.Expedition.New.Types
import Data.Int

resourceScorer :: forall f. (ResourcePerHr -> Number) -> Scorer f
resourceScorer f rph _ = f rph

simpleResourceScorer :: forall f. ResourceRows Number -> Scorer f
simpleResourceScorer p = resourceScorer scorer
  where
    scorer rph = rph.fuel * p.fuel
               + rph.ammo * p.ammo
               + rph.steel * p.steel
               + rph.bauxite * p.bauxite

-- TODO: 1. calc fleet net income for one exped
-- 2. sum up with others
calcResourcePerHr :: FleetNetIncome -> Info -> Int -> ResourcePerHr
calcResourcePerHr (FNI ni) info afkTimeInMin = 
    { fuel: perHr ni.fuel
    , ammo: perHr ni.ammo
    , steel: perHr ni.steel
    , bauxite: perHr ni.bauxite
    }
  where
    t = max afkTimeInMin info.timeInMin
    perHr :: Int -> Number
    perHr x = toNumber (60 * x) / toNumber t
