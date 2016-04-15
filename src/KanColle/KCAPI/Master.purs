module KanColle.KCAPI.Master where

-- support for api_start2
type Master =
  { api_mst_ship :: Array MstShip
  , api_mst_shipupgrade :: Array MstShipUpgrade
  }

type MstShip =
  { api_name :: String
  , api_id :: Int
  , api_afterlv :: Int
  , api_afterfuel :: Int
  , api_afterbull :: Int
  , api_aftershipid :: String
  }

type MstShipUpgrade =
  { api_id :: Int
  , api_current_ship_id :: Int
  , api_catapult_count :: Int
  , api_drawing_count :: Int
  }
