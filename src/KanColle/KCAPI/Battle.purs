module KanColle.KCAPI.Battle where

import Prelude
import Data.Maybe
import Data.Foreign
import Data.Nullable
import Data.Foreign.Index
import qualified Data.Array.Unsafe as AU

-- things might actually be "null" or even "undefined"
-- we choose to:
-- * trust flag api_opening_flag for opening torpedo attacks
-- * test property existency in other cases

-- TODO: change this to foreign, or hide the definition
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
  , api_hougeki :: Hougeki
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
hasField s = hasOwnProperty s <<< toForeign

hasKouku :: Battle -> Boolean
hasKouku = hasField "api_stage_flag"

hasKouku2 :: Battle -> Boolean
hasKouku2 = hasField "api_stage_flag2"

hasHourai :: Battle -> Boolean
hasHourai = hasField "api_hourai_flag"

hasHougeki :: Battle -> Boolean
hasHougeki = hasField "api_hougeki"

getKouku :: Battle -> Maybe Kouku
getKouku b = if hasKouku b
  then Just b.api_kouku
  else Nothing

getKouku2 :: Battle -> Maybe Kouku
getKouku2 b = if hasKouku2 b
  then Just b.api_kouku2
  else Nothing

getOpeningAttack :: Battle -> Maybe Raigeki
getOpeningAttack b = 
    if hasField "api_opening_flag" b
    && b.api_opening_flag == 1
      then Just b.api_opening_atack
      else Nothing

getHougeki1 :: Battle -> Maybe Hougeki
getHougeki1 b = if hasField "api_hougeki1" b
  then Just b.api_hougeki1
  else Nothing

getHougeki2 :: Battle -> Maybe Hougeki
getHougeki2 b = if hasField "api_hougeki2" b
  then Just b.api_hougeki2
  else Nothing

getHougeki3 :: Battle -> Maybe Hougeki
getHougeki3 b = if hasField "api_hougeki3" b
  then Just b.api_hougeki3
  else Nothing

getRaigeki :: Battle -> Maybe Raigeki
getRaigeki b = if hasField "api_raigeki" b
  then Just b.api_raigeki
  else Nothing

getHougeki :: Battle -> Maybe Hougeki
getHougeki b = if hasHougeki b
  then Just b.api_hougeki
  else Nothing
