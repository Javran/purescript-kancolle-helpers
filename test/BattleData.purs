module BattleData where

import Prelude
import Data.Foreign
import KanColle.KCAPI.Battle

foreign import battle1 :: Battle
foreign import battle2 :: Battle

foreign import nightBattle1 :: Battle
foreign import nightBattle2 :: Battle

foreign import aerialBattle1 :: Battle

foreign import withSupportExpedition1 :: Battle
foreign import withSupportExpedition2 :: Battle

foreign import surfaceTaskForceBattle1 :: Battle
foreign import carrierTaskForceBattle1 :: Battle
foreign import carrierTaskForceBattle2 :: Battle
foreign import carrierTaskForceAirBattle1 :: Battle

foreign import combinedFleetNightBattle1 :: Battle
-- sample data for a HP array of length less than 13 (on combined fleet)
-- it's very likely that terminating "-1"s are truncated.
foreign import combinedFleetNightBattle2 :: Battle

foreign import surfaceTaskForceBattleWithSupport1 :: Battle

foreign import ldAerialBattle1 :: Battle
foreign import ldAerialBattle2 :: Battle

foreign import nightBattleWithDameCon1 :: Battle
foreign import normBattleWithDameCon1 :: Battle

foreign import normBattleWithLandBasedAir1 :: Battle
foreign import normBattleWithLandBasedAir2 :: Battle
foreign import combinedFleetWithLandBasedAir1 :: Battle

foreign import normBattleWithOpeningTaisen1 :: Battle

foreign import combinedFleetOpeingTaisenSTF1 :: Battle
foreign import combinedFleetOpeingTaisenCTF1 :: Battle

foreign import abyssalCombinedFleet1 :: Battle
foreign import abyssalCombinedFleetNight1 :: Battle
foreign import abyssalCombinedFleetNight2 :: Battle

foreign import bothCombinedCTF1 :: Battle
