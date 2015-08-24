module KanColle.Expedition.Requirement where

import Prelude
import Data.Array
import Data.Foldable
import Data.Maybe
import Data.Monoid
import qualified Data.String as Str

import KanColle.SType

data ShipRequirement
  = ShipLevel Int
  | ShipTypeOneOf (Array SType)

data FleetRequirement
  = Flagship ShipRequirement
  | FleetLevel Int
  | FleetDrum Int
  | FleetShipWithDrum Int
  | FleetSTypeCount Int (Array SType)
  | FleetShipCount Int

type ExpeditionRequirement = Array FleetRequirement

explainShipRequirement :: ShipRequirement -> String
explainShipRequirement v = case v of
    ShipLevel lvl -> "ship level should be at least " <> show lvl
    ShipTypeOneOf stypes ->
      "ship should have one of the following type: " <> Str.joinWith "," (map showSType stypes)

explainFleetRequirement :: FleetRequirement -> String
explainFleetRequirement v = case v of
    Flagship fs -> "flagship: " <> explainShipRequirement fs
    FleetLevel lvl -> "fleet level sum should be at least " <> show lvl
    FleetDrum drum -> "fleet should have at least " <> show drum <> " drum(s)"
    FleetShipWithDrum n -> "at least " <> show n <> " ship(s) should have drum(s)"
    FleetSTypeCount n stypes -> "require at least "
      <> show n <> " ship(s) of the following type: " <> Str.joinWith "," (map showSType stypes)
    FleetShipCount n -> "fleet should have at least " <> show n <> " ship(s)"

type Ship a =
  { ammo :: Int
  , morale :: Int
  , stype :: SType
  , level :: Int
  , drumCount :: Int
  | a}

type RawShip a =
  { ammo :: Int
  , morale :: Int
  , stype :: String
  , level :: Int
  , drumCount :: Int
  | a}

type Fleet a = Array (Ship a)

fromRawShip :: forall a. RawShip a -> Ship a
fromRawShip s = s {stype = readSType s.stype}

fromRawFleet :: forall a. Array (RawShip a) -> Fleet a
fromRawFleet = map fromRawShip

checkShip :: forall a. ShipRequirement -> Ship a -> Boolean
checkShip req s = case req of
  ShipLevel n -> s.level >= n
  ShipTypeOneOf stype -> any ((==) s.stype) stype

checkFleet :: forall a. FleetRequirement -> Fleet a -> Boolean
checkFleet req fleet = case uncons fleet of
    Just { head: hd, tail: tl } -> case req of
        Flagship req -> checkShip req hd
        FleetLevel n -> sum (map (\x -> x.level) fleet) >= n
        FleetDrum n -> sum (map (\x -> x.drumCount) fleet) >= n
        FleetShipWithDrum n -> count (\x -> x.drumCount > 0) fleet >= n
        FleetSTypeCount n stypes -> count (checkShip (ShipTypeOneOf stypes)) fleet >= n
        FleetShipCount n -> length fleet >= n
    Nothing -> false
  where
    count pred = length <<< filter pred

getExpeditionRequirement :: Int -> ExpeditionRequirement
getExpeditionRequirement v = case v of
    1 -> fslAndSc 1 2
    2 -> fslAndSc 2 4
    3 -> fslAndSc 3 3
    4 -> fslAndSc 3 3 <> sty 1 CL <> sty 2 DD
    5 -> fslAndSc 3 4 <> sty 1 CL <> sty 2 DD
    6 -> fslAndSc 4 4
    7 -> fslAndSc 5 6
    8 -> fslAndSc 6 6

    9 -> fslAndSc 3 4 <> sty 1 CL <> sty 2 DD
    10 -> fslAndSc 3 3 <> sty 2 CL
    11 -> fslAndSc 6 4 <> sty 2 DD
    12 -> fslAndSc 4 4 <> sty 2 DD
    13 -> fslAndSc 5 6 <> sty 1 CL <> sty 4 DD
    14 -> fslAndSc 6 6 <> sty 1 CL <> sty 3 DD
    15 -> fslAndSc 9 6 <> carrier 2 <> sty 2 DD
    16 -> fslAndSc 11 6 <> sty 1 CL <> sty 2 DD

    17 -> fslAndSc 20 6 <> sty 1 CL <> sty 3 DD
    18 -> fslAndSc 15 6 <> carrier 3 <> sty 2 DD
    19 -> fslAndSc 20 6 <> sty 2 BBV <> sty 2 DD
    20 -> fslAndSc 1 2 <> submarine 1 <> sty 1 CL
    21 -> fslAndSc 15 5 <> lvlCnt 30 <> sty 1 CL <> sty 4 DD
       <> [FleetShipWithDrum 3]
    22 -> fslAndSc 30 6 <> lvlCnt 45 <> sty 1 CA <> sty 1 CL <> sty 2 DD
    23 -> fslAndSc 50 6 <> lvlCnt 200 <> sty 2 BBV <> sty 2 DD
    24 -> fslAndSc 50 6 <> lvlCnt 200 <> sty 1 CL <> sty 4 DD
       <> [Flagship (ShipTypeOneOf [CL])]

    25 -> fslAndSc 25 4 <> sty 2 CA <> sty 2 DD
    26 -> fslAndSc 30 4 <> carrier 1 <> sty 1 CL <> sty 2 DD
    27 -> fslAndSc 1 2 <> submarine 2
    28 -> fslAndSc 30 3 <> submarine 3
    29 -> fslAndSc 50 3 <> submarine 3
    30 -> fslAndSc 55 4 <> submarine 4
    31 -> fslAndSc 60 4 <> lvlCnt 200 <> submarine 4
    32 -> fslAndSc 5 3 <> sty 1 CT <> sty 2 DD
       <> [Flagship (ShipTypeOneOf [CT])]

    35 -> fslAndSc 40 6 <> carrier 2 <> sty 1 CA <> sty 1 DD
    36 -> fslAndSc 30 6 <> sty 2 AV <> sty 1 CL <> sty 1 DD
    37 -> fslAndSc 50 6 <> lvlCnt 200 <> sty 1 CL <> sty 5 DD
       <> [FleetShipWithDrum 4]
    38 -> fslAndSc 65 6 <> lvlCnt 240 <> sty 5 DD
       <> [FleetShipWithDrum 4, FleetDrum 8]
    39 -> fslAndSc 3 5 <> lvlCnt 180 <> sty 1 AS <> submarine 4
    40 -> fslAndSc 25 6 <> lvlCnt 150 <> sty 1 CL <> sty 2 AV <> sty 2 DD
       <> [Flagship (ShipTypeOneOf [CL])]
    _ -> []
  where
    lvlCnt n = [FleetLevel n]
    carrier n = [FleetSTypeCount n [CV,CVL,AV]]
    submarine n = [FleetSTypeCount n [SS,SSV]]
    sty n stype = [FleetSTypeCount n [stype]]
    fslAndSc fsl sc =
      [Flagship (ShipLevel fsl), FleetShipCount sc]

unsatisfiedRequirements :: forall a. Int -> Fleet a -> Array FleetRequirement
unsatisfiedRequirements eId fleet =
    filter (\fr -> not (checkFleet fr fleet)) fleetReqs
  where
    fleetReqs = getExpeditionRequirement eId

explainRequirements :: Array FleetRequirement -> Array String
explainRequirements = map explainFleetRequirement
