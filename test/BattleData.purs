module BattleData where

import Prelude
import Data.Foreign
import KanColle.DamageAnalysis

foreign import battle1 :: Foreign
foreign import battle2 :: Foreign

foreign import nightBattle1 :: Foreign

battle1Result :: String
battle1Result = pprFleetDamageTookInfo $ analyzeRawBattle battle1

battle2Result :: String
battle2Result = pprFleetDamageTookInfo $ analyzeRawBattle battle2

nightBattle1Result :: String
nightBattle1Result = pprFleetDamageTookInfoNight $ analyzeRawNightBattle nightBattle1
