module KanColle.Expedition.Consumption where

import Prelude
import Data.Int

type Consumption =
  { fuel :: Number
  , ammo :: Number
  , time :: Int -- in minutes
  }

noConsumption :: Consumption
noConsumption = { fuel: 0.0, ammo: 0.0, time: 0 }

getExpeditionConsumption :: Int -> Consumption
getExpeditionConsumption eId = case eId of
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
    24 -> c 8 6 $ hm 8 20

    25 -> c 5 8 $ hr 40
    26 -> c 8 8 $ hr 80
    27 -> c 8 8 $ hr 20
    28 -> c 8 8 $ hr 25
    29 -> c 9 4 $ hr 24
    30 -> c 9 7 $ hr 48
    31 -> c 5 0 $ hr 2
    32 -> c 9 3 $ hr 24

    35 -> c 8 8 $ hr 7
    36 -> c 8 8 $ hr 9
    37 -> c 8 8 $ hm 2 45
    38 -> c 8 8 $ hm 2 55
    39 -> c 9 9 $ hr 30
    40 -> c 8 7 $ hm 6 50
    _ -> noConsumption
  where
    hr = (* 60)
    hm h m = h*60 + m
    c f a t = { fuel: percent f, ammo: percent a, time: t }
    percent v = toNumber (v * 10) / 100.0
