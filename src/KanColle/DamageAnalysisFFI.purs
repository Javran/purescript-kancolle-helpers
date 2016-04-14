module KanColle.DamageAnalysisFFI where

import Prelude
import KanColle.Util
import KanColle.DamageAnalysis.Damage
import KanColle.DamageAnalysis2
import KanColle.KCAPI.Battle
import Data.Foreign
import Data.Maybe
import Data.Array
import Data.Function
import Data.Nullable

readDameCon :: Array Int -> Array (Maybe DameCon)
readDameCon = map convert
  where
    convert :: Int -> Maybe DameCon
    convert 0 = Nothing
    convert 1 = Just RepairTeam
    convert 2 = Just RepairGodness
    convert x = throwWith ("readDameCon: invalid input: " <> show x)

dc6 :: Array (Maybe DameCon)
dc6 = replicate 6 Nothing

dc12 :: Array (Maybe DameCon)
dc12 = replicate 12 Nothing

analyzeBattleJS :: Fn2 (Array Int) Battle { main :: Array (Nullable ShipResult)
                                          , enemy :: Array (Nullable ShipResult)}
analyzeBattleJS = mkFn2 (\ds b -> convert (analyzeBattle (readDameCon ds) b))
  where
    convert s = { main: map toNullable s.main
                , enemy: map toNullable s.enemy }

analyzeNightBattleJS :: Fn2 (Array Int) Battle { main :: Array (Nullable ShipResult)
                                               , enemy :: Array (Nullable ShipResult)}
analyzeNightBattleJS = mkFn2 (\ds b -> convert (analyzeNightBattle (readDameCon ds) b))
  where
    convert s = { main: map toNullable s.main
                , enemy: map toNullable s.enemy }
