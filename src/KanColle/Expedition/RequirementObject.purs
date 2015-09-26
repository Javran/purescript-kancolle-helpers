module KanColle.Expedition.RequirementObject where

import Prelude
import Data.Foldable
import Data.Maybe
import Data.Array
import Data.Nullable

import KanColle.SType
import KanColle.Expedition.Base
import KanColle.Expedition.Requirement

type STypeReq =
  { stypeReqCount :: Int
  , stypeOneOf :: Array SType
  }

type STypeReqObj =
  { stypeReqCount :: Int
  , stypeOneOf :: Array String
  }
  
-- compact requirement representation
type RequirementPack =
  { flagShipLevel :: Int
  , shipCount :: Int
  , flagShipTypeOf :: Maybe (Array SType)
  , levelCount :: Maybe Int
  , drumCount :: Maybe Int 
  , drumCarrierCount :: Maybe Int
  , fleetSType :: Array STypeReq
  }

type RequirementObject =
  { flagShipLevel :: Int
  , shipCount :: Int
  , flagShipTypeOf :: Nullable (Array String)
  , levelCount :: Nullable Int
  , drumCount :: Nullable Int 
  , drumCarrierCount :: Nullable Int
  , fleetSType :: Array STypeReqObj
  }

dummyRequirementPack :: RequirementPack
dummyRequirementPack =
    { flagShipLevel: 1
    , shipCount: 1
    , flagShipTypeOf: Nothing
    , levelCount: Nothing
    , drumCount: Nothing
    , drumCarrierCount: Nothing
    , fleetSType: []
    }

fromExpeditionRequirement :: ExpeditionRequirement -> RequirementPack
fromExpeditionRequirement = foldl updatePack dummyRequirementPack
  where
    updatePack p fleetReq = case fleetReq of
        Flagship sr ->
          case sr of
            ShipLevel l -> p { flagShipLevel = l }
            ShipTypeOneOf sts -> p { flagShipTypeOf = Just sts }
        FleetLevel l -> p { levelCount = Just l }
        FleetDrum d -> p { drumCount = Just d }
        FleetShipWithDrum swd -> p { drumCarrierCount = Just swd }
        FleetSTypeCount sCount sts ->
          let req = { stypeReqCount: sCount
                    , stypeOneOf: sts }
          in p { fleetSType = snoc p.fleetSType req }
        FleetShipCount c -> p { shipCount = c }

-- the point of having this is to go from PureScript world to JavaScript
packToObj :: RequirementPack -> RequirementObject
packToObj rp =
    { flagShipLevel: rp.flagShipLevel
    , shipCount: rp.shipCount
    , flagShipTypeOf: toNullable ((map >>> map) showSType rp.flagShipTypeOf)
    , levelCount: toNullable rp.levelCount
    , drumCount: toNullable rp.drumCount
    , drumCarrierCount: toNullable rp.drumCarrierCount
    , fleetSType: map cov rp.fleetSType -- TODO
    }
  where
    cov :: STypeReq -> STypeReqObj
    cov stReq = 
        { stypeReqCount: stReq.stypeReqCount
        , stypeOneOf: map showSType stReq.stypeOneOf
        }

allRequirements :: Array { id :: Int,  val :: RequirementObject }
allRequirements = map mk allExpeditionIds
  where
    mk v = { id: v
           , val: (packToObj <<< fromExpeditionRequirement) 
                    (getExpeditionRequirement v)
           }
