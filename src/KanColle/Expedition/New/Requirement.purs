module KanColle.Expedition.New.Requirement where

import Prelude
import Data.Maybe
import Data.Monoid.Endo
import Data.Map as M
import KanColle.Expedition.New.SType

type CompoReq = M.Map SType Int

addReq :: SType -> Int -> Endo CompoReq
addReq s n = Endo (M.alter f s)
  where
    f = maybe n (_+n) >>> Just
