module KanColle.Expedition.Minimal where

import Prelude
import Data.Monoid
import Data.Array
import Data.Foldable

import KanColle.SType

import KanColle.Expedition
import KanColle.Expedition.Requirement
import KanColle.Util

type ShipMaxCost =
  { fuel :: Int
  , ammo :: Int
  }

-- | minimal cost of an expedition, just for the purpose of estimation
-- | despite that we are trying to provide a possible ship composition
-- | that minimizes the cost, the suggestion might not actually be possible
-- | due to many factors (e.g. other running expeditions are using the same ship,
-- | or you have not yet obtained the ship)
newtype ECost = ECost
  { shipCost :: Array ShipMaxCost
  , fleet :: Fleet ()
  }

instance eCostSemigroup :: Semigroup ECost where
  append (ECost a) (ECost b) =
      ECost { shipCost: a.shipCost <> b.shipCost
            , fleet: a.fleet <> b.fleet }

instance eCostMonoid :: Monoid ECost where
  mempty = ECost { shipCost: mempty, fleet: mempty }


dummyShip :: SType -> Ship ()
dummyShip s =
    { ammo: 400, morale: 100
    , stype: s, level: 150
    , drumCount: 4 }

mkECost :: SType -> Int -> Int -> ECost
mkECost s f a = ECost
    { shipCost: [{ fuel: f, ammo: a }]
    , fleet: [dummyShip s] }

-- | minimal DD cost, achivable by taking any of Mutsuki class ships
ddCost :: Int -> ECost
ddCost n = times n mutsukiClass
  where
    mutsukiClass = mkECost DD 15 15

-- | minimal SS/SSV cost, achivable by taking
-- | Maruyu and I-168 (Kai) / I-58 / I-19 / I-8 / U-511 / Ro-500
ssCost :: Int -> ECost
ssCost n = case n of
    1 -> maruyu
    _ -> maruyu <> times (n-1) nonSSV
  where
    maruyu = mkECost SS 10 10
    nonSSV = mkECost SS 10 20

-- | minimal CVL cost, achivable by taking
-- | Houshou / Shouhou class (without Kai).
-- | Input is expected to be within [1..3]
-- | as the minimal expedition
-- | never requires more than 3 CVLs at a time
cvlCost :: Int -> ECost
cvlCost n = case n of
    1 -> houshou
    _ -> houshou <> times (n-1) shouhou
  where
    houshou = mkECost CVL 30 30
    shouhou = mkECost CVL 35 35

-- | minimal AV cost, input takes value [1..3]
avCost :: Int -> ECost
avCost n = if n <= 2
    then times n chitose
    else times 2 chitose <> (mkECost AV 60 15)
  where
    chitose = mkECost AV 35 35

-- | minimal BBV cost, input takes value [1..4]
-- | achivable by taking any non-Kai-Ni BBV
bbvCost :: Int -> ECost
bbvCost n = times n (mkECost BBV 95 105)

-- | minimal CA cost, achivable by taking
-- | Furutaka class + Aoba class, takes [1..4]
caCost :: Int -> ECost
caCost n = times n (mkECost CA 35 55)

-- | minimal CL cost, achivable by taking
-- | Tenryuu class + non-Kai Kuma class
clCost :: Int -> ECost
clCost n = times n (mkECost CL 25 25)

-- | the sole submarine tender (AS) is Taigei
taigei :: ECost
taigei = mkECost AS 35 10

-- | the sole training cruiser (CT) is Katori
katori :: ECost
katori = mkECost CT 35 20

-- | fill in submarines to meet a ship number requirement
fillSS :: Int -> ECost -> ECost
fillSS n v@(ECost ec) = if l >= n
    then v
    else v <> ssCost (n-l)
  where
    l = length ec.fleet

getExpeditionMinCost :: Int -> ECost
getExpeditionMinCost v = case v of
    1 -> ssCost 2
    2 -> ssCost 4
    3 -> ssCost 3
    4 -> clCost 1 <> ddCost 2
    5 -> fillSS 4 (clCost 1 <> ddCost 2)
    6 -> ssCost 4
    7 -> ssCost 6
    8 -> ssCost 6

    9 -> fillSS 4 (clCost 1 <> ddCost 2)
    10 -> fillSS 3 (clCost 2)
    11 -> fillSS 4 (ddCost 2)
    12 -> fillSS 4 (ddCost 2)
    13 -> fillSS 6 (clCost 1 <> ddCost 4)
    14 -> fillSS 6 (clCost 1 <> ddCost 3)
    15 -> fillSS 6 (cvlCost 2 <> ddCost 2)
    16 -> fillSS 6 (clCost 1 <> ddCost 2)

    17 -> fillSS 6 (clCost 1 <> ddCost 3)
    18 -> fillSS 6 (cvlCost 3 <> ddCost 2)
    19 -> fillSS 6 (bbvCost 2 <> ddCost 2)
    20 -> ssCost 1 <> clCost 1
    21 -> clCost 1 <> ddCost 4
    22 -> fillSS 6 (caCost 1 <> clCost 1 <> ddCost 2)
    23 -> fillSS 6 (bbvCost 2 <> ddCost 2)
    24 -> fillSS 6 (clCost 1 <> ddCost 4)

    25 -> caCost 2 <> ddCost 2
    26 -> cvlCost 1 <> clCost 1 <> ddCost 2
    27 -> ssCost 2
    28 -> ssCost 3
    29 -> ssCost 3
    30 -> ssCost 4
    31 -> ssCost 4
    32 -> katori <> ddCost 2

    35 -> fillSS 6 (cvlCost 2 <> caCost 1 <> ddCost 1)
    36 -> fillSS 6 (avCost 2 <> clCost 1 <> ddCost 1)
    37 -> clCost 1 <> ddCost 5
    38 -> fillSS 6 (ddCost 5)
    39 -> taigei <> ssCost 4
    40 -> fillSS 6 (clCost 1 <> avCost 2 <> ddCost 2)

    _ -> mempty
