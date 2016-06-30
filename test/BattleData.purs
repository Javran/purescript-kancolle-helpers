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

foreign import surfaceTaskForceBattleWithSupport1 :: Battle

foreign import ldAerialBattle1 :: Battle
foreign import ldAerialBattle2 :: Battle

foreign import nightBattleWithDameCon1 :: Battle
foreign import normBattleWithDameCon1 :: Battle

foreign import normBattleWithLandBasedAir1 :: Battle
foreign import normBattleWithLandBasedAir2 :: Battle
foreign import combinedFleetWithLandBasedAir1 :: Battle

foreign import normBattleWithOpeningTaisen1 :: Battle
