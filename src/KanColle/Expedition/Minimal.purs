module KanColle.Expedition.Minimal where

import Prelude
import Data.Monoid
import Data.Array
import Data.Array.Partial as PA
import Data.Foldable
import Data.String as Str

import KanColle.SType

import KanColle.Expedition.Base
import KanColle.Expedition.Cost
import KanColle.Expedition.Requirement
import KanColle.Util
import Data.Maybe
import Partial.Unsafe

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
  , fleet :: Fleet (note :: Maybe String)
  }
  
getECost :: ECost -> { shipCost :: Array ShipMaxCost
                     , fleet :: Fleet (note :: Maybe String)}
getECost (ECost ec) = ec
  
instance eCostSemigroup :: Semigroup ECost where
  append (ECost a) (ECost b) =
      ECost { shipCost: a.shipCost <> b.shipCost
            , fleet: a.fleet <> b.fleet }

instance eCostMonoid :: Monoid ECost where
  mempty = ECost { shipCost: mempty, fleet: mempty }

dummyShip :: SType -> Ship (note :: Maybe String)
dummyShip s =
    { ammo: 400, morale: 100
    , stype: s, level: 150
    , drumCount: 4
    , note: Nothing }
    
shipWithNote :: SType -> String -> Ship (note :: Maybe String)
shipWithNote s n = (dummyShip s) { note= Just n }

mkECost :: SType -> Int -> Int -> ECost
mkECost s f a = ECost
    { shipCost: [{ fuel: f, ammo: a }]
    , fleet: [dummyShip s]
    }
    
mkECostWithNote :: SType -> Int -> Int -> String -> ECost
mkECostWithNote s f a n = ECost
    { shipCost: [{ fuel: f, ammo: a }]
    , fleet: [shipWithNote s n]
    }

-- | minimal DD cost, achivable by taking any of Mutsuki class ships
ddCost :: Int -> ECost
ddCost n = times n mutsukiClass
  where
    mutsukiClass = mkECostWithNote DD 15 15 "睦月级(改/改二皆可)"

-- | minimal SS/SSV cost, achivable by taking
-- | Maruyu and I-168 (Kai) / I-58 / I-19 / I-8 / U-511 / Ro-500
ssCost :: Int -> ECost
ssCost n = case n of
    1 -> maruyu
    _ -> maruyu <> times (n-1) nonSSV
  where
    maruyu = mkECostWithNote SS 10 10 "马战神"
    nonSSV = mkECostWithNote SS 10 20 "非马战神SS"

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
    houshou = mkECostWithNote CVL 25 25 "凤翔"
    shouhou = mkECostWithNote CVL 35 35 "祥凤级"

-- | minimal AV cost, input takes value [1..4]
avCost :: Int -> ECost
avCost n = if n <= 3
    then
      times n chitoseOrMizuho
    else
      -- since the input <= 4, this must be the branch where n == 4
      times 3 chitoseOrMizuho <> akitsushima
  where
    chitoseOrMizuho = mkECostWithNote AV 35 35 "千岁级/瑞穗"
    akitsushima = mkECostWithNote AV 50 10 "秋津洲"

-- | minimal BBV cost, input takes value [1..4]
-- | achivable by taking any non-Kai-Ni BBV
bbvCost :: Int -> ECost
bbvCost n = times n fusouOrIse
  where
    fusouOrIse = mkECostWithNote BBV 95 105 "扶桑级改/伊势级改"

-- | minimal CA cost, achivable by taking
-- | Furutaka class + Aoba class, takes [1..4]
caCost :: Int -> ECost
caCost n = times n (mkECostWithNote CA 35 50 "古鹰级/青叶级")

-- | minimal CL cost, achivable by taking
-- | Tenryuu class + non-Kai Kuma class
clCost :: Int -> ECost
clCost n = if n <= 2
    then times n tenryuu
    else times 2 tenryuu <> times (n-2) kuma
  where
    tenryuu = mkECostWithNote CL 25 20 "天龙级"
    kuma = mkECostWithNote CL 25 25 "球磨级"

-- | the sole submarine tender (AS) is Taigei
taigei :: ECost
taigei = mkECostWithNote AS 35 10 "大鲸"

-- | the sole training cruiser (CT) is Katori
katori :: ECost
katori = mkECostWithNote CT 35 20 "香取"

-- | fill in submarines to meet a ship number requirement
fillSS :: Int -> ECost -> ECost
fillSS n v@(ECost ec) = if l >= n
    then v
    else v <> ssCost (n-l)
  where
    l = length ec.fleet

getExpeditionMinCost :: Int -> ECost
getExpeditionMinCost v = case v of
    1 -> fillSS 2 mempty
    2 -> fillSS 4 mempty
    3 -> fillSS 3 mempty
    4 -> clCost 1 <> ddCost 2
    5 -> fillSS 4 (clCost 1 <> ddCost 2)
    6 -> fillSS 4 mempty
    7 -> fillSS 6 mempty
    8 -> fillSS 6 mempty

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

    33 -> ddCost 2
    34 -> ddCost 2
    35 -> fillSS 6 (cvlCost 2 <> caCost 1 <> ddCost 1)
    36 -> fillSS 6 (avCost 2 <> clCost 1 <> ddCost 1)
    37 -> clCost 1 <> ddCost 5
    38 -> fillSS 6 (ddCost 5)
    39 -> taigei <> ssCost 4
    40 -> fillSS 6 (clCost 1 <> avCost 2 <> ddCost 2)

    _ -> mempty

pprFleetNotes :: Fleet (note :: Maybe String) -> String
pprFleetNotes xs = if null notes
    then "<N/A>"
    else
      let grouppedNotes = map formatNote (group notes)
          formatNote note = "{" <> unsafePartial (PA.head note) <> "}x" <> show (length note)
      in Str.joinWith ", " grouppedNotes
  where
    notes = mapMaybe (\x -> x.note) xs

-- | generate markdown string for pretty printing the minimal cost table
minimalCostMarkdownTable :: String
minimalCostMarkdownTable = header <> "\n" <> Str.joinWith "\n" rows
  where
    header = "Expedition Id | Fuel | Ammo | Minimal Cost Fleet Composition" <> "\n"
          <> " --- | --- | --- | ---"
    rows = map mkRow allExpeditionIds
    mkRow eId = Str.joinWith " | " [show eId, show fuelCosts, show ammoCosts, notes]
      where
        notes = pprFleetNotes minCost.fleet
        cost = getExpeditionCost eId
        minCost = getECost (getExpeditionMinCost eId)
        costVec = map (\sc -> calcCost cost sc) minCost.shipCost
        fuelCosts = sum (map (\x -> x.fuel) costVec)
        ammoCosts = sum (map (\x -> x.ammo) costVec)
