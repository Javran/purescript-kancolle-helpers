module KanColle.KCAPI.Battle where

import Data.Foreign
import Data.Array
import Data.Nullable

-- things might actually be "null",
-- we choose to trust flags, i.e. api_stage_flag and api_hourai_flag
type Battle =
  { api_nowhps :: Array Int
  , api_stage_flag :: Array Int
  , api_kouku :: Kouku
  , api_opening_flag :: Int
  , api_opening_atack :: Raigeki
  , api_hourai_flag :: Array Int
  , api_hougeki1 :: Hougeki
  , api_hougeki2 :: Hougeki
  , api_hougeki3 :: Hougeki
  , api_raigeki :: Raigeki
  }

type NightBattle =
  { api_nowhps :: Array Int
  , api_hougeki :: Hougeki
  }

type Kouku =
  { api_stage3 :: KoukuStage3
  }

type KoukuStage3 =
  { api_fdam :: Array Number
  , api_edam :: Array Number
  }

type Hougeki =
  { api_df_list :: Array Foreign
  , api_damage :: Array Foreign
  }

type Raigeki =
  { api_fdam :: Array Number
  , api_edam :: Array Number
  }
