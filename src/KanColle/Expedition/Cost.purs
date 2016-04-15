module KanColle.Expedition.Cost
  ( Cost(..)
  , calcCost
  , getExpeditionCost
  ) where
  
-- this module serves as a database about expedition's cost

import Prelude
import Data.Int

-- | Expedition cost. `fuel` and `ammo` are floating numbers
-- | (valid values are taken from `0.0` to `1.0`) representing
-- | percentage of fuel / ammo running an expedition would cost.
-- | And `time` records the total time (in minutes) of running this expedition.
type Cost =
  { fuel :: Number
  , ammo :: Number
  , time :: Int -- | in minutes
  }

noCost :: Cost
noCost = { fuel: 0.0, ammo: 0.0, time: 0 }

-- TODO: need document, or it looks confusing
-- TODO: integrate other parts to use this function
calcCost :: Cost 
         -> { fuel :: Int, ammo :: Int }
         -> { fuel :: Int, ammo :: Int }
calcCost c x = { fuel: floor (toNumber x.fuel * c.fuel)
               , ammo: floor (toNumber x.ammo * c.ammo) }

-- | input a valid expedition id and get expedition cost.
getExpeditionCost :: Int -> Cost
getExpeditionCost eId = case eId of
    1 ->  c 3 0   15
    2 ->  c 5 0   30
    3 ->  c 3 2   20
    4 ->  c 5 0   50
    5 ->  c 5 0 $ hm 1 30
    6 ->  c 3 2   40
    7 ->  c 5 0 $ hr 1
    8 ->  c 5 2 $ hr 3

    9 ->  c 5 0 $ hr 4
    10 -> c 3 0 $ hm 1 30
    11 -> c 5 0 $ hr 5
    12 -> c 5 0 $ hr 8
    13 -> c 5 4 $ hr 4
    14 -> c 5 0 $ hr 6
    15 -> c 5 4 $ hr 12
    16 -> c 5 4 $ hr 15

    17 -> c 3 4 $ 45
    18 -> c 5 2 $ hr 5
    19 -> c 5 4 $ hr 6
    20 -> c 5 4 $ hr 2
    21 -> c 8 7 $ hm 2 20
    22 -> c 8 7 $ hr 3
    23 -> c 8 8 $ hr 4
    24 -> c 9 6 $ hm 8 20

    25 -> c 5 8 $ hr 40
    26 -> c 8 8 $ hr 80
    27 -> c 8 8 $ hr 20
    28 -> c 8 8 $ hr 25
    29 -> c 9 4 $ hr 24
    30 -> c 9 7 $ hr 48
    31 -> c 5 0 $ hr 2
    32 -> c 9 3 $ hr 24

    33 -> c 5 8   15
    34 -> c 5 8   30
    35 -> c 8 8 $ hr 7
    36 -> c 8 8 $ hr 9
    37 -> c 8 8 $ hm 2 45
    38 -> c 8 8 $ hm 2 55
    39 -> c 9 9 $ hr 30
    40 -> c 8 7 $ hm 6 50
    _ -> noCost
  where
    hr = (_ * 60)
    hm h m = h*60 + m
    c f a t = { fuel: percent f, ammo: percent a, time: t }
    percent v = toNumber (v * 10) / 100.0
