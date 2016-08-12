module KanColle.Expedition.New.Info where

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
