module KanColle.Expedition.New.Info
  ( getInformation
  , expedInfoTable
  ) where

import KanColle.Expedition.New.EArray
import KanColle.Expedition.New.Types
import Data.Array.Partial as AP
import Partial.Unsafe
import Data.Maybe
import Prelude

-- informations about all expeditions
type RawInfo =
  { api_id :: Int
  , api_time :: Int
  , api_use_fuel :: Number
  , api_use_bull :: Number
  , api_win_item1 :: Array Int
  , api_win_item2 :: Array Int
  }

foreign import kcExpeditionRaw :: Array RawInfo

getInformation :: Int -> Info
getInformation = indEA expedInfoTable

expedInfoTable :: EArray Info
expedInfoTable = mkEA (convertRawInfo <$> kcExpeditionRaw)

convertRawInfo :: RawInfo -> Info
convertRawInfo ri =
    { id: ri.api_id
    , timeInMin: ri.api_time
    , fuelCostPercent: ri.api_use_fuel
    , ammoCostPercent: ri.api_use_bull
    , item1: convertRawItem ri.api_win_item1
    , item2: convertRawItem ri.api_win_item2
    }
  where
    convertRawItem ar = case (unsafePartial AP.unsafeIndex ar 0) of
      0 -> Nothing
      itmCode -> Just
        { item: itemFromInt itmCode
        , maxCount: (unsafePartial AP.unsafeIndex ar 1)
        }
              
