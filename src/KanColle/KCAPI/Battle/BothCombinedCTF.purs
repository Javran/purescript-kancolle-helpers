module KanColle.KCAPI.Battle.BothCombinedCTF
  ( getHougeki1
  , getRaigeki
  , getHougeki2
  , getHougeki3  
  ) where

import KanColle.KCAPI.Battle.Internal

import Prelude
import Data.Maybe

getHougeki1 :: Battle -> Maybe Hougeki
getHougeki1 b@(Battle rb) = do
    checkHouraiFlag 0 b
    pure rb.api_hougeki1

getHougeki2 :: Battle -> Maybe Hougeki
getHougeki2 b@(Battle rb) = do
    checkHouraiFlag 1 b
    pure rb.api_hougeki2
    
getRaigeki :: Battle -> Maybe Raigeki
getRaigeki b@(Battle rb) = do
    checkHouraiFlag 2 b
    pure rb.api_raigeki

getHougeki3 :: Battle -> Maybe Hougeki
getHougeki3 b@(Battle rb) = do
    checkHouraiFlag 3 b
    pure rb.api_hougeki3
