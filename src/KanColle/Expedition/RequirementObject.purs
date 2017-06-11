module KanColle.Expedition.RequirementObject where

import Prelude
import Data.Foldable
import Data.Maybe
import Data.Array hiding (length)
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

type ResultPack (f :: Type -> Type) =
  { flagShipLevel :: Boolean
  , shipCount :: Boolean
  , flagShipTypeOf :: f Boolean
  , levelCount :: f Boolean
  , drumCount :: f Boolean
  , drumCarrierCount :: f Boolean
  , fleetSType :: Array Boolean
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

type ResultObject = ResultPack Nullable

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
requirementPackToObj :: RequirementPack -> RequirementObject
requirementPackToObj rp =
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
           , val: getExpeditionRequirementObject v
           }

getExpeditionRequirementPack :: Int -> RequirementPack
getExpeditionRequirementPack eId = fromExpeditionRequirement (getExpeditionRequirement eId)

getExpeditionRequirementObject :: Int -> RequirementObject
getExpeditionRequirementObject = requirementPackToObj <<< getExpeditionRequirementPack

checkWithRequirementPack :: forall a. RequirementPack -> Fleet a -> ResultPack Maybe
checkWithRequirementPack req fleet = case uncons fleet of
    Just { head: fs, tail: tl } ->
      let flagShipLevel = fs.level >= req.flagShipLevel
      in { flagShipLevel: flagShipLevel
         , shipCount: length fleet >= req.shipCount
         , flagShipTypeOf:
             map (checkShipType fs.stype) req.flagShipTypeOf
         , levelCount:
             map (\lc -> sum (map (\s -> s.level) fleet) >= lc) req.levelCount
         , drumCount:
             map (\dc -> sum (map (\s -> s.drumCount) fleet) >= dc) req.drumCount
         , drumCarrierCount:
             map (\dcc -> count (\s -> s.drumCount > 0) fleet >= dcc) req.drumCarrierCount
         , fleetSType:
             map (\r ->
                     count
                       (\s -> checkShipType s.stype r.stypeOneOf)
                       fleet
                     >= r.stypeReqCount) req.fleetSType
         }
    Nothing ->
      let toFalseF :: forall b f. (Functor f) => f b -> f Boolean
          toFalseF = map (const false)
      in { flagShipLevel: false
         , shipCount: false
         , flagShipTypeOf: toFalseF req.flagShipTypeOf
         , levelCount: toFalseF req.levelCount
         , drumCount: toFalseF req.drumCount
         , drumCarrierCount: toFalseF req.drumCarrierCount
         , fleetSType: toFalseF req.fleetSType
         }
  where
    checkShipType :: SType -> Array SType -> Boolean
    checkShipType st = any (st == _)
    count pred = length <<< filter pred

resultPackToObject :: ResultPack Maybe -> ResultPack Nullable
resultPackToObject rp = rp
    { flagShipTypeOf = toNullable rp.flagShipTypeOf
    , levelCount = toNullable rp.levelCount
    , drumCount = toNullable rp.drumCount
    , drumCarrierCount = toNullable rp.drumCarrierCount
    }
