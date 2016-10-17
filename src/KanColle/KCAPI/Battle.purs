module KanColle.KCAPI.Battle
  ( Battle
  , KArray, fromKArray -- eventually we should remove this ...

  , SupportAirInfo
  , getSupportAirInfo

  , SupportHouraiInfo
  , getSupportHouraiInfo

  , getLandBasedAirStrikes

  , Kouku
  , getKouku
  , getKouku2

  , KoukuStage3
  , getKoukuStage3
  , getKoukuStage3FDam
  , getKoukuStage3EDam

  , Hougeki
  , getHougeki

  , getHougeki1
  , getHougeki1AC
  , getHougeki1CT

  , getHougeki2
  , getHougeki2AC
  , getHougeki2CT

  , getHougeki3
  , getHougeki3AC
  , getHougeki3CT

  , getOpeningTaisen

  , Raigeki
  , getRaigeki
  , getRaigekiCT
  , getRaigekiAC
  , getOpeningAttack

  , getInitHps
  , getInitHpsCombined
  , getMaxHps
  , getMaxHpsCombined
  ) where

import Prelude
import Data.Maybe
import Data.Foreign
import Data.Foreign.Index
import Control.MonadPlus
import KanColle.Util

-- | `KArray` is a special kind of array that KCAPI uses a lot,
-- | it's first element is always "-1" so that the first element
-- | in fact the second element, this makes "arr[i]" literally means the i-th element
newtype KArray a = KArray (Array a)

-- | remove leading "-1" so the array is zero-indexed
fromKArray :: forall a. KArray a -> Array a
fromKArray (KArray ar) = unsafeArrTail ar

{-
  this module serves as "field accessing" purposes.

  - for example, "Battle" definition contains "api_kouku2"
    but not all data of type "Battle" will always have this type,
    it's just convenient doing this rather than have distinct types
    for different kind of similar structures
-}

-- things might actually be "null" or even "undefined"
-- we choose to:
-- * trust flag api_opening_flag for opening torpedo attacks
-- * test property existency in other cases

-- TODO: change this to foreign, or hide the definition, the idea is to make "Battle"
-- structure not accessible outside this module
-- TODO: maybe eventually we'll make almost everything invisible
-- to reduce the possibility of accidentally accessing non-existing stuff.
type RawBattle =
  { api_nowhps :: KArray Int
  , api_nowhps_combined :: KArray Int
  , api_maxhps :: KArray Int
  , api_maxhps_combined :: KArray Int
  , api_stage_flag :: Array Int
  , api_stage_flag2 :: Array Int
  , api_kouku :: Kouku
  , api_kouku2 :: Kouku
  , api_opening_taisen_flag :: Int
  , api_opening_taisen :: Hougeki

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
  , api_air_base_attack :: Array Kouku
  }

newtype Battle = Battle RawBattle

-- TODO: instead of getHougeki<some stuff>
data BattleType
  = BNormal
  | BCTF
  | BSTF
  | BTECF
  | BAbyCTF

type Kouku =
  { api_stage_flag :: Array Int
  , api_stage3 :: KoukuStage3
  , api_stage3_combined :: KoukuStage3
  }

-- WARNING: keep in mind that combined fleet does not have an "api_edam" field
type KoukuStage3 =
  { api_fdam :: KArray Number
  , api_edam :: KArray Number
  }

type Hougeki =
  { api_df_list :: KArray (Array Int)
  , api_at_eflag :: KArray Int
  , api_damage :: KArray (Array Number)
  }

type Raigeki =
  { api_fdam :: KArray Number
  , api_edam :: KArray Number
  }

type SupportAirInfo =
  { api_stage3 :: KoukuStage3
  , api_stage3_combined :: KoukuStage3 }

type SupportHouraiInfo =
  { api_damage :: KArray Number }

type SupportInfo =
  { api_support_airatack :: SupportAirInfo
  , api_support_hourai :: SupportHouraiInfo
  }

fromRawHp :: Int -> Maybe Int
fromRawHp -1 = Nothing
fromRawHp v = Just v

getInitHps :: Battle -> Array (Maybe Int)
getInitHps (Battle rb) = map fromRawHp $ fromKArray rb.api_nowhps

getInitHpsCombined :: Battle -> Array (Maybe Int)
getInitHpsCombined (Battle rb) = map fromRawHp $ fromKArray rb.api_nowhps_combined

getMaxHps :: Battle -> Array (Maybe Int)
getMaxHps (Battle rb) = map fromRawHp $ fromKArray rb.api_maxhps

getMaxHpsCombined :: Battle -> Array (Maybe Int)
getMaxHpsCombined (Battle rb) = map fromRawHp $ fromKArray rb.api_maxhps_combined

hasField :: forall a. String -> a -> Boolean
hasField s = hasOwnProperty s <<< toForeign

hasKouku :: Battle -> Boolean
hasKouku (Battle rb) | hasField "api_stage_flag" rb =
          unsafeArrIndex rb.api_stage_flag 2 == 1
hasKouku _ = false

hasKouku2 :: Battle -> Boolean
hasKouku2 (Battle rb) | hasField "api_stage_flag2" rb =
          unsafeArrIndex rb.api_stage_flag2 2 == 1
hasKouku2 _ = false

hasHourai :: Battle -> Boolean
hasHourai (Battle rb) = hasField "api_hourai_flag" rb

hasHougeki :: Battle -> Boolean
hasHougeki (Battle rb) = hasField "api_hougeki" rb

getKouku :: Battle -> Maybe Kouku
getKouku b@(Battle rb) = if hasKouku b
  then Just rb.api_kouku
  else Nothing

getSupportFlag :: Battle -> Maybe Int
getSupportFlag b@(Battle rb) = if hasField "api_support_flag" b
  then Just rb.api_support_flag
  else Nothing

getSupportAirInfo :: Battle -> Maybe SupportAirInfo
getSupportAirInfo b@(Battle rb) = do
    i <- getSupportFlag b
    guard (i == 1)
    pure rb.api_support_info.api_support_airatack

getSupportHouraiInfo :: Battle -> Maybe SupportHouraiInfo
getSupportHouraiInfo b@(Battle rb) = do
    i <- getSupportFlag b
    guard $ i == 2 || i == 3
    pure rb.api_support_info.api_support_hourai

getKouku2 :: Battle -> Maybe Kouku
getKouku2 b@(Battle rb) = if hasKouku2 b
  then Just rb.api_kouku2
  else Nothing

getHouraiFlags :: Battle -> Maybe (Array Int)
getHouraiFlags b@(Battle rb) =
   if hasField "api_hourai_flag" b
     then Just rb.api_hourai_flag
     else Nothing

checkHouraiFlag :: Int -> Battle -> Maybe Unit
checkHouraiFlag ind b = do
    flg <- (_ `unsafeArrIndex` ind) <$> getHouraiFlags b
    guard (flg == 1)

getOpeningAttack :: Battle -> Maybe Raigeki
getOpeningAttack b@(Battle rb) =
    if hasField "api_opening_flag" b
    && rb.api_opening_flag == 1
      then Just rb.api_opening_atack
      else Nothing

getOpeningTaisen :: Battle -> Maybe Hougeki
getOpeningTaisen b@(Battle rb) =
    if hasField "api_opening_taisen_flag" b
    && rb.api_opening_taisen_flag == 1
      then Just rb.api_opening_taisen
      else Nothing

getHougeki1 :: Battle -> Maybe Hougeki
getHougeki1 b@(Battle rb) = do
    checkHouraiFlag 0 b
    pure rb.api_hougeki1

getHougeki2 :: Battle -> Maybe Hougeki
getHougeki2 b@(Battle rb) = do
    checkHouraiFlag 1 b
    pure rb.api_hougeki2

getHougeki3 :: Battle -> Maybe Hougeki
getHougeki3 b@(Battle rb) = do
    checkHouraiFlag 2 b
    pure rb.api_hougeki3

getRaigeki :: Battle -> Maybe Raigeki
getRaigeki b@(Battle rb) = do
    checkHouraiFlag 3 b
    pure rb.api_raigeki

getHougeki :: Battle -> Maybe Hougeki
getHougeki b@(Battle rb) = if hasHougeki b
  then Just rb.api_hougeki
  else Nothing

-- TODO: unify this with other functions
-- Carrier Task Force is using a different ordering.
-- specalized as "CT" here.
getHougeki1CT :: Battle -> Maybe Hougeki
getHougeki1CT b@(Battle rb) = do
    checkHouraiFlag 0 b
    pure rb.api_hougeki1

getRaigekiCT :: Battle -> Maybe Raigeki
getRaigekiCT b@(Battle rb) = do
    checkHouraiFlag 1 b
    pure rb.api_raigeki

getHougeki2CT :: Battle -> Maybe Hougeki
getHougeki2CT b@(Battle rb) = do
    checkHouraiFlag 2 b
    pure rb.api_hougeki2

getHougeki3CT :: Battle -> Maybe Hougeki
getHougeki3CT b@(Battle rb) = do
    checkHouraiFlag 3 b
    pure rb.api_hougeki3

getHougeki1AC :: Battle -> Maybe Hougeki
getHougeki1AC = getHougeki1CT

getRaigekiAC :: Battle -> Maybe Raigeki
getRaigekiAC = getRaigekiCT

getHougeki2AC :: Battle -> Maybe Hougeki
getHougeki2AC = getHougeki2CT

getHougeki3AC :: Battle -> Maybe Hougeki
getHougeki3AC = getHougeki3CT

hasLandBasedAirStrikes :: Battle -> Boolean
hasLandBasedAirStrikes = hasField "api_air_base_attack"

getLandBasedAirStrikes :: Battle -> Maybe (Array Kouku)
getLandBasedAirStrikes b@(Battle rb) = if hasLandBasedAirStrikes b
    then Just rb.api_air_base_attack
    else Nothing

getKoukuStage3 :: Kouku -> Maybe KoukuStage3
getKoukuStage3 kk = if unsafeArrIndex kk.api_stage_flag 2 == 1
    then Just kk.api_stage3
    else Nothing

-- fancy way of encoding a pair
getKoukuStage3AC :: forall r. Kouku -> Maybe ((KoukuStage3 -> KoukuStage3 -> r) -> r)
getKoukuStage3AC kk = if unsafeArrIndex kk.api_stage_flag 2 == 1
    then Just (\f -> f kk.api_stage3 kk.api_stage3_combined)
    else Nothing

getKoukuStage3EEscortMaybe :: Kouku -> Maybe KoukuStage3
getKoukuStage3EEscortMaybe kk = if unsafeArrIndex kk.api_stage_flag 2 == 1
    then Just kk.api_stage3_combined
    else Nothing

getKoukuStage3FDam :: KoukuStage3 -> Maybe (KArray Number)
getKoukuStage3FDam ks3 = if hasField "api_fdam" ks3
    then Just ks3.api_fdam
    else Nothing

getKoukuStage3EDam :: KoukuStage3 -> Maybe (KArray Number)
getKoukuStage3EDam ks3 = if hasField "api_edam" ks3
    then Just ks3.api_edam
    else Nothing
