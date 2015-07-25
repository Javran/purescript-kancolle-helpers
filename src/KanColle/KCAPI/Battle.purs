module KanColle.KCAPI.Battle where

import Data.Foreign
import Data.Nullable
import Data.Foreign.Index

-- things might actually be "null",
-- we choose to trust flags, i.e. api_stage_flag and api_hourai_flag
type Battle =
  { api_nowhps :: Array Int
  , api_stage_flag :: Array Int
  , api_stage_flag2 :: Array Int
  , api_kouku :: Kouku
  , api_kouku2 :: Kouku
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

hasField :: String -> Battle -> Boolean
hasField s b = hasOwnProperty s (toForeign b)

hasKouku :: Battle -> Boolean
hasKouku = hasField "api_stage_flag"

hasKouku2 :: Battle -> Boolean
hasKouku2 = hasField "api_stage_flag2"

hasHourai :: Battle -> Boolean
hasHourai = hasField "api_hourai_flag"
