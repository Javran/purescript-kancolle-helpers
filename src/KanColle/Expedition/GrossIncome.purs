module KanColle.Expedition.GrossIncome
  ( IncomeFactor(..)
  , GrossIncome()
  , getGrossIncome
  , withFactorToGrossIncome
  , toGrossIncome
  , showGrossIncome
  ) where

import Prelude
import KanColle.Expedition.Base
import KanColle.Expedition.IncomeBase
import Data.Int

-- | `IncomeFactor` keeps track of factors that would
-- | affect expedition income
type IncomeFactor =
  { greatSuccess :: Boolean -- is great success?
  , landingCraft :: Int -- number of landing crafts (INVARIANT: >= 0)
  }

-- | no great success, without any landing craft
defaultFactor :: IncomeFactor
defaultFactor =
  { greatSuccess: false
  , landingCraft: 0
  }

newtype GrossIncome = GrossIncome (ResourceRows Int)

-- | access gross income fields
getGrossIncome :: GrossIncome -> ResourceRows Int
getGrossIncome (GrossIncome v) = v

-- | calculate gross income based on `IncomeBase`
-- | together with some factors
withFactorToGrossIncome :: IncomeFactor -> IncomeBase -> GrossIncome
withFactorToGrossIncome f b = GrossIncome (mapResourceRows modify ib)
  where
    ib = getIncomeBase b
    normLandingCraft =
        if f.landingCraft >= 4
          then 4
          else f.landingCraft
    lcFactor = 1.0 + (toNumber normLandingCraft) * 0.05
    gsFactor = if f.greatSuccess then 1.5 else 1.0
    modify = toNumber
         >>> (_ * lcFactor)
         >>> (_ * gsFactor)
         >>> floor

-- | calculate gross income based on `IncomeBase`
-- | without extra factors
toGrossIncome :: IncomeBase -> GrossIncome
toGrossIncome = withFactorToGrossIncome defaultFactor

-- | visualize `GrossIncome` as a `String`
showGrossIncome :: GrossIncome -> String
showGrossIncome (GrossIncome v) =
        "GrossIncome ("
     <> show v.fuel <> ", "
     <> show v.ammo <> ", "
     <> show v.steel <> ", "
     <> show v.bauxite <> ")"
