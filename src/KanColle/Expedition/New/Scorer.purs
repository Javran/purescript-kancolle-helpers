module KanColle.Expedition.New.Scorer where

import Prelude
import KanColle.Expedition.Base
import KanColle.Expedition.New.Types

resourceScorer :: forall f. (ResourcePerHr -> Number) -> Scorer f
resourceScorer f rph _ = f rph

simpleResourceScorer :: forall f. ResourceRows Number -> Scorer f
simpleResourceScorer p = resourceScorer scorer
  where
    scorer rph = rph.fuel * p.fuel
               + rph.ammo * p.ammo
               + rph.steel * p.steel
               + rph.bauxite * p.bauxite
