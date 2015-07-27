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
hasKouku b | hasField "api_stage_flag" b =
          AU.unsafeIndex b.api_stage_flag 2 == 1
hasKouku _ = false          

hasKouku2 :: Battle -> Boolean
hasKouku2 b | hasField "api_stage_flag2" b = AU.unsafeIndex b.api_stage_flag2 2 == 1
hasKouku2 _ = false

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
getHougeki1 b
  | hasField "api_hougeki1" b = if 
               AU.unsafeIndex b.api_hourai_flag 0 == 1
  then Just b.api_hougeki1
  else Nothing
getHougeki1 _ = Nothing

getHougeki2 :: Battle -> Maybe Hougeki
getHougeki2 b
  | hasField "api_hougeki2" b = if
                AU.unsafeIndex b.api_hourai_flag 1 == 1
  then Just b.api_hougeki2
  else Nothing
getHougeki2 _ = Nothing

getHougeki3 :: Battle -> Maybe Hougeki
getHougeki3 b
  | hasField "api_hougeki3" b = if
                AU.unsafeIndex b.api_hourai_flag 2 == 1
  then Just b.api_hougeki3
  else Nothing
getHougeki3 _ = Nothing  

getRaigeki :: Battle -> Maybe Raigeki
getRaigeki b
  | hasField "api_raigeki" b = if
                AU.unsafeIndex b.api_hourai_flag 3 == 1
  then Just b.api_raigeki
  else Nothing
getRaigeki _ = Nothing    

getHougeki :: Battle -> Maybe Hougeki
getHougeki b = if hasHougeki b
  then Just b.api_hougeki
  else Nothing
