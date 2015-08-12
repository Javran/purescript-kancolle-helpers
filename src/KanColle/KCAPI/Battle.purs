module KanColle.KCAPI.Battle where

import Prelude
import Data.Maybe
import Data.Foreign
import Data.Nullable
import Data.Foreign.Index
import Control.MonadPlus
import qualified Data.Array.Unsafe as AU

-- things might actually be "null" or even "undefined"
-- we choose to:
-- * trust flag api_opening_flag for opening torpedo attacks
-- * test property existency in other cases

-- TODO: change this to foreign, or hide the definition
type Battle =
  { api_nowhps :: Array Int
  , api_nowhps_combined :: Array Int
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
  , api_support_flag :: Int
  , api_support_info :: SupportInfo
  }

type Kouku =
  { api_stage3 :: KoukuStage3
  , api_stage3_combined :: KoukuStage3
  }

-- WARNING: keep in mind that combined fleet does not have an "api_edam" field
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

type SupportAirInfo =
  { api_stage3 :: { api_edam :: Array Number } }

type SupportHouraiInfo =
  { api_damage :: Array Number }

type SupportInfo =
  { api_support_airatack :: SupportAirInfo
  , api_support_hourai :: SupportHouraiInfo
  }

getInitHps :: Battle -> Array (Maybe Int)
getInitHps b = map fromRawHp b.api_nowhps
  where
    fromRawHp -1 = Nothing
    fromRawHp v = Just v

getInitHpsCombined :: Battle -> Array (Maybe Int)
getInitHpsCombined b = map fromRawHp b.api_nowhps_combined
  where
    fromRawHp -1 = Nothing
    fromRawHp v = Just v

hasField :: String -> Battle -> Boolean
hasField s = hasOwnProperty s <<< toForeign

hasKouku :: Battle -> Boolean
hasKouku b | hasField "api_stage_flag" b =
          AU.unsafeIndex b.api_stage_flag 2 == 1
hasKouku _ = false

hasKouku2 :: Battle -> Boolean
hasKouku2 b | hasField "api_stage_flag2" b =
          AU.unsafeIndex b.api_stage_flag2 2 == 1
hasKouku2 _ = false

hasHourai :: Battle -> Boolean
hasHourai = hasField "api_hourai_flag"

hasHougeki :: Battle -> Boolean
hasHougeki = hasField "api_hougeki"

getKouku :: Battle -> Maybe Kouku
getKouku b = if hasKouku b
  then Just b.api_kouku
  else Nothing

getSupportFlag :: Battle -> Maybe Int
getSupportFlag b = if hasField "api_support_flag" b
  then Just b.api_support_flag
  else Nothing

getSupportAirInfo :: Battle -> Maybe SupportAirInfo
getSupportAirInfo b = do
    i <- getSupportFlag b
    guard (i == 1)
    return b.api_support_info.api_support_airatack

getSupportHouraiInfo :: Battle -> Maybe SupportHouraiInfo
getSupportHouraiInfo b = do
    i <- getSupportFlag b
    guard $ i == 2 || i == 3
    return b.api_support_info.api_support_hourai

getKouku2 :: Battle -> Maybe Kouku
getKouku2 b = if hasKouku2 b
  then Just b.api_kouku2
  else Nothing

getHouraiFlags :: Battle -> Maybe (Array Int)
getHouraiFlags b =
   if hasField "api_hourai_flag" b
     then Just b.api_hourai_flag
     else Nothing

checkHouraiFlag :: Int -> Battle -> Maybe Unit
checkHouraiFlag ind b = do
    flg <- (`AU.unsafeIndex` ind) <$> getHouraiFlags b
    guard (flg == 1)

getOpeningAttack :: Battle -> Maybe Raigeki
getOpeningAttack b =
    if hasField "api_opening_flag" b
    && b.api_opening_flag == 1
      then Just b.api_opening_atack
      else Nothing

getHougeki1 :: Battle -> Maybe Hougeki
getHougeki1 b = do
    checkHouraiFlag 0 b
    return b.api_hougeki1

getHougeki2 :: Battle -> Maybe Hougeki
getHougeki2 b = do
    checkHouraiFlag 1 b
    return b.api_hougeki2

getHougeki3 :: Battle -> Maybe Hougeki
getHougeki3 b = do
    checkHouraiFlag 2 b
    return b.api_hougeki3

getRaigeki :: Battle -> Maybe Raigeki
getRaigeki b = do
    checkHouraiFlag 3 b
    return b.api_raigeki

getHougeki :: Battle -> Maybe Hougeki
getHougeki b = if hasHougeki b
  then Just b.api_hougeki
  else Nothing

-- TODO: unify this with other functions
-- Carrier Task Force is using a different ordering.
-- specalized as "CT" here.
getHougeki1CT :: Battle -> Maybe Hougeki
getHougeki1CT b = do
    checkHouraiFlag 0 b
    return b.api_hougeki1

getRaigekiCT :: Battle -> Maybe Raigeki
getRaigekiCT b = do
    checkHouraiFlag 1 b
    return b.api_raigeki

getHougeki2CT :: Battle -> Maybe Hougeki
getHougeki2CT b = do
    checkHouraiFlag 2 b
    return b.api_hougeki2

getHougeki3CT :: Battle -> Maybe Hougeki
getHougeki3CT b = do
    checkHouraiFlag 3 b
    return b.api_hougeki3
