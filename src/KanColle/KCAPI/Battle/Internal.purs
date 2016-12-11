module KanColle.KCAPI.Battle.Internal
  ( RawBattle
  , Kouku
  , KoukuStage3
  , Hougeki
  , Raigeki
  , SupportInfo
  , SupportAirInfo
  , SupportHouraiInfo
  
  , Battle(..)
  
  , KArray, fromKArray -- eventually we should remove this ...
  
  , getHouraiFlags
  , checkHouraiFlag
  , hasField
  ) where

import Prelude
import Data.Maybe
import Control.MonadPlus

import Data.Foreign
import Data.Foreign.Index

import KanColle.Util

-- | `KArray` is a special kind of array that KCAPI uses a lot,
-- | it's first element is always "-1" so that the first element
-- | in fact the second element, this makes "arr[i]" literally means the i-th element
newtype KArray a = KArray (Array a)

-- | remove leading "-1" so the array is zero-indexed
fromKArray :: forall a. KArray a -> Array a
fromKArray (KArray ar) = unsafeArrTail ar

-- this module is meant for other modules within KanColle.KCAPI.Battle
-- and should never be used elsewhere.

-- TODO: change this to foreign, or hide the definition, the idea is to make "Battle"
-- structure not accessible outside this module
-- TODO: maybe eventually we'll make almost everything invisible
-- to reduce the possibility of accidentally accessing non-existing stuff.
type RawBattle =
  { api_active_deck :: Array Int -- for abyssal combined fleet
  , api_nowhps :: KArray Int
  , api_nowhps_combined :: KArray Int
  , api_maxhps :: KArray Int
  , api_maxhps_combined :: KArray Int
  , api_stage_flag :: Array Int
  , api_stage_flag2 :: Array Int
  , api_injection_kouku :: Kouku
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
  , api_air_base_injection :: Kouku
  }

newtype Battle = Battle RawBattle

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

hasField :: forall a. String -> a -> Boolean
hasField s = hasOwnProperty s <<< toForeign

getHouraiFlags :: Battle -> Maybe (Array Int)
getHouraiFlags b@(Battle rb) =
   if hasField "api_hourai_flag" b
     then Just rb.api_hourai_flag
     else Nothing

checkHouraiFlag :: Int -> Battle -> Maybe Unit
checkHouraiFlag ind b = do
    flg <- (_ `unsafeArrIndex` ind) <$> getHouraiFlags b
    guard (flg == 1)
