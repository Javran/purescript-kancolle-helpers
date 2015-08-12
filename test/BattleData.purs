module BattleData where

import Prelude
import Data.Foreign
import KanColle.DamageAnalysis
import KanColle.DamageAnalysis.Stages

foreign import battle1 :: Foreign
foreign import battle2 :: Foreign

foreign import nightBattle1 :: Foreign
foreign import nightBattle2 :: Foreign

foreign import aerialBattle1 :: Foreign

foreign import withSupportExpedition1 :: Foreign
foreign import withSupportExpedition2 :: Foreign

foreign import surfaceTaskForceBattle1 :: Foreign
foreign import carrierTaskForceBattle1 :: Foreign
foreign import carrierTaskForceAirBattle1 :: Foreign

foreign import combinedFleetNightBattle1 :: Foreign

battle1Result :: String
battle1Result = pprFleetDamageTookInfo $ analyzeRawBattle battle1

battle2Result :: String
battle2Result = pprFleetDamageTookInfo $ analyzeRawBattle battle2

nightBattle1Result :: String
nightBattle1Result = pprFleetDamageTookInfoNight $ analyzeRawNightBattle nightBattle1

nightBattle2Result :: String
nightBattle2Result = pprFleetDamageTookInfoNight $ analyzeRawNightBattle nightBattle2

aerialBattle1Result :: String
aerialBattle1Result = pprFleetDamageTookInfo $ analyzeRawBattle aerialBattle1

supportHouraiDebug1 :: String
supportHouraiDebug1 = show $ supportHouraiDV (unsafeFromForeign withSupportExpedition1)

supportHouraiDebug2 :: String
supportHouraiDebug2 = show $ supportHouraiDV (unsafeFromForeign withSupportExpedition2)
