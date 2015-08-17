module KanColle.RepairTime where

import Prelude
import Math hiding (round,floor,ceil)
import Data.Int
import KanColle.SType
import Data.Maybe
import Data.Function

dockingInSec :: SType -> Int -> Int -> Int -> Int
dockingInSec s lvl curHp maxHp = fromMaybe 0 (fromNumber repairTime) + baseTime
  where
    lostHp = maxHp - curHp
    a = floor (sqrt (toNumber (lvl - 11))) * 10 + 50
    baseTime = 30
    repairTime :: Number
    repairTime = if lvl <= 11
        then toNumber (lvl * 10) * stypeFactor s * toNumber lostHp
        else toNumber (lvl * 5 + a) * stypeFactor s * toNumber lostHp

dockingInSecJS :: Fn4 String Int Int Int Int
dockingInSecJS = mkFn4 (\s -> dockingInSec (readSType s))

facilityInSec :: SType -> Int -> Int -> Int -> Int
facilityInSec s lvl curHp maxHp = if facilitySlowestTime <= roundDockingTime
    then facilitySlowestTime
    else roundDockingTime
  where
    m20 = 20*60
    dockingTime = dockingInSec s lvl curHp maxHp
    roundDockingTime :: Int
    roundDockingTime = ceil (toNumber dockingTime / toNumber m20) * m20
    facilitySlowestTime = m20 * (maxHp - curHp)

facilityInSecJS :: Fn4 String Int Int Int Int
facilityInSecJS = mkFn4 (\s -> facilityInSec (readSType s))

stypeFactor :: SType -> Number
stypeFactor SS = 0.5

stypeFactor DD = 1.0
stypeFactor CL = 1.0
stypeFactor CLT = 1.0
stypeFactor CT = 1.0
stypeFactor AV = 1.0
stypeFactor SSV = 1.0
stypeFactor LHA = 1.0

stypeFactor CA = 1.5
stypeFactor CAV = 1.5
stypeFactor FBB = 1.5
stypeFactor CVL = 1.5
stypeFactor AS = 1.5

stypeFactor BB = 2.0
stypeFactor BBV = 2.0
stypeFactor CV = 2.0
stypeFactor CVB = 2.0
stypeFactor AR = 2.0

-- the following 3 types are still unconfirmed
-- so I'll just guess.
stypeFactor DDE = 1.0
stypeFactor XBB = 2.0
stypeFactor AP = 2.0
