module KanColle.Expedition.Income
  ( Income()
  , mkIncome
  , getIncome
  , getExpeditionIncome
  , withGreatSuccess
  ) where

-- TODO: great success requires a 6-ships-fleet, which means the minimum cost
-- is not precise enough

-- this module serves as a database about the resource
-- running an expedition would get.
-- note that "great success" is not taken into account.

import Prelude
import Data.Monoid
import qualified Data.Int as DI

-- | `Income` includes `fuel`, `ammo` `steel` and `bauxite` resource income.
newtype Income = Income
  { fuel :: Int
  , ammo :: Int
  , steel :: Int
  , bauxite :: Int
  , greatSuccess :: Boolean
  , landingCrafts :: Int -- number of landing crafts
  }

-- | unwrap `Income` to expose its members
getIncome :: Income -> { fuel :: Int
                       , ammo :: Int
                       , steel :: Int
                       , bauxite :: Int
                       }
getIncome (Income i) =
    { fuel: i.fuel
    , ammo: i.ammo
    , steel: i.steel
    , bauxite: i.bauxite
    }

mkIncome :: { fuel :: Int, ammo :: Int, steel :: Int, bauxite :: Int } -> Income
mkIncome i = income i.fuel i.ammo i.steel i.bauxite

income :: Int -> Int -> Int -> Int -> Income
income f a s b = Income { fuel: f
                        , ammo: a
                        , steel: s
                        , bauxite: b
                        , greatSuccess: false
                        , landingCrafts: 0
                        }

withGreatSuccess :: Income -> Income
withGreatSuccess (Income i) =
    income
      (toGS i.fuel)
      (toGS i.ammo)
      (toGS i.steel)
      (toGS i.bauxite)

toGS :: Int -> Int
toGS = DI.toNumber >>> (* 1.5) >>> DI.floor

instance incomeSemigroup :: Semigroup Income where
    append (Income i1) (Income i2) =
        income (i1.fuel + i2.fuel)
               (i1.ammo + i2.ammo)
               (i1.steel + i2.steel)
               (i1.bauxite + i2.bauxite)

instance incomeMonoid :: Monoid Income where
    mempty = income 0 0 0 0

-- | input a valid expedition id and get expedition income.
getExpeditionIncome :: Int -> Income
getExpeditionIncome eId = case eId of
    1 ->  i   0  30   0   0
    2 ->  i   0 100  30   0
    3 ->  i  30  30  40   0
    4 ->  i   0  60   0   0
    5 ->  i 200 200  20  20
    6 ->  i   0   0   0  80
    7 ->  i   0   0  50  30
    8 ->  i  50 100  50  50

    9 ->  i 350   0   0   0
    10 -> i   0  50   0  30
    11 -> i   0   0   0 250
    12 -> i  50 250 200  50
    13 -> i 240 300   0   0
    14 -> i   0 240 200   0
    15 -> i   0   0 300 400
    16 -> i 500 500 200 200

    17 -> i  70  70  50   0
    18 -> i   0   0 300 100
    19 -> i 400   0  50  30
    20 -> i   0   0 150   0
    21 -> i 320 270   0   0
    22 -> i   0  10   0   0
    23 -> i   0  20   0 100
    24 -> i 500   0   0 150

    25 -> i 900   0 500   0
    26 -> i   0   0   0 900
    27 -> i   0   0 800   0
    28 -> i   0   0 900 350
    29 -> i   0   0   0 100
    30 -> i   0   0   0 100
    31 -> i   0  30   0   0
    32 -> i  50  50  50  50

    35 -> i   0   0 240 280
    36 -> i 480   0 200 200
    37 -> i   0 380 270   0
    38 -> i 420   0 200   0
    39 -> i   0   0 300   0
    40 -> i 300 300   0 100

    _ -> mempty
  where
    i = income
