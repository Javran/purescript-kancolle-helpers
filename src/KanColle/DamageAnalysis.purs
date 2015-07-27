module KanColle.DamageAnalysis where

import Prelude
import Data.Maybe
import Data.Maybe.Unsafe
import KanColle.KCAPI.Battle
import Data.Array
import Data.Foreign
import Data.Int
import Data.Monoid
import Data.Monoid.Endo
import Data.Nullable
import Data.Foldable
import Data.String (joinWith)
import Math
import qualified Data.Array.Unsafe as AU
import KanColle.DamageAnalysis.DamageVector
import KanColle.DamageAnalysis.Stages

-- information about damage took by a ship
type DamageTookInfo =
  { currentHp :: Int
  }

type DamageTookInfoNight =
  { currentHp :: Int
  }

pprDamageTookInfo :: DamageTookInfo -> String
pprDamageTookInfo dti
  = "DamageInfo {"
 <> "currentHp=" <> show dti.currentHp
 <> "}"

pprDamageTookInfoNight :: DamageTookInfoNight -> String
pprDamageTookInfoNight dti
  = "DamageInfoNight {"
 <> "currentHp=" <> show dti.currentHp
 <> "}"

pprFleetDamageTookInfo :: AllFleetInfo DamageTookInfo -> String
pprFleetDamageTookInfo xs = joinWith " | " $ map (maybe "<N/A>" pprDamageTookInfo) xs

pprFleetDamageTookInfoNight :: AllFleetInfo DamageTookInfoNight -> String
pprFleetDamageTookInfoNight xs = joinWith " | " $ map (maybe "<N/A>" pprDamageTookInfoNight) xs


-- invariant: the length is always 1+12,
-- index 0 is never used
-- index 1-6 for frient units
-- index 7-12 for enemies
type AllFleetInfo a = Array (Maybe a)

type DamageAnalyzer = Battle
                   -> AllFleetInfo DamageTookInfo
                   -> AllFleetInfo DamageTookInfo

noDamage :: Int -> DamageTookInfo
noDamage hp =
  { currentHp: hp
  }

-- initialize a battle, does nothing but setup currentHp
battleStart :: Battle -> AllFleetInfo DamageTookInfo
battleStart b = hpList
  where
    hpList = map fromRawHp b.api_nowhps
    fromRawHp -1 = Nothing
    fromRawHp v = Just (noDamage v)

battleStartNight :: NightBattle -> AllFleetInfo DamageTookInfoNight
battleStartNight b = hpList
  where
    hpList = map fromRawHp b.api_nowhps
    fromRawHp -1 = Nothing
    fromRawHp v = Just {currentHp: v}

damageNormalize :: Array Number -> Array Int
damageNormalize = map (fromJust <<< fromNumber <<< floor)

calcAerial :: DamageAnalyzer
calcAerial b xs | hasKouku b =
 if AU.unsafeIndex b.api_stage_flag 2 == 0
    then xs
    else
      let k :: KoukuStage3
          k = b.api_kouku.api_stage3
          fDam :: Array Int
          fDam = AU.tail $ damageNormalize k.api_fdam
          eDam :: Array Int
          eDam = AU.tail $ damageNormalize k.api_edam
          combine :: Int -> DamageTookInfo -> DamageTookInfo
          combine dmg dti = dti { currentHp=dti.currentHp - dmg }
          damages = [0] <> fDam <> eDam
      in zipWith (map <<< combine) damages xs
calcAerial _ xs = xs

calcAerial2 :: DamageAnalyzer
calcAerial2 b xs | hasKouku2 b =
  if AU.unsafeIndex b.api_stage_flag2 2 == 0
    then xs
    else
      let k :: KoukuStage3
          k = b.api_kouku2.api_stage3
          fDam :: Array Int
          fDam = AU.tail $ damageNormalize k.api_fdam
          eDam :: Array Int
          eDam = AU.tail $ damageNormalize k.api_edam
          combine :: Int -> DamageTookInfo -> DamageTookInfo
          combine dmg dti = dti { currentHp=dti.currentHp - dmg }
          damages = [0] <> fDam <> eDam
      in zipWith (map <<< combine) damages xs
calcAerial2 _ xs = xs

-- TODO: refactor.
calcHougeki :: Hougeki
            -> Int -- indicates which one, one of [1,2,3]
            -> AllFleetInfo DamageTookInfo
            -> AllFleetInfo DamageTookInfo
calcHougeki h i xs = zipWith (map <<< combine) fleetDamages xs
  where
    convert1 :: Foreign -> Array Int
    convert1 = unsafeFromForeign
    convert2 :: Foreign -> Array Number
    convert2 = unsafeFromForeign
    targets :: Array Int
    targets = concat (map convert1 (AU.tail h.api_df_list))
    damages :: Array Int
    damages = damageNormalize (concat (map convert2 (AU.tail h.api_damage)))
    -- summarize damage here
    fleetDamages :: Array Int
    fleetDamages = runEndo dmgModifiers $ replicate 13 0
      where
        dmgModifiers :: Endo (Array Int)
        dmgModifiers = mconcat $ zipWith combine targets damages
          where
            combine t d = Endo $ fromJust <<< modifyAt t (+ d)
    combine :: Int -> DamageTookInfo -> DamageTookInfo
    combine dmg dti = case i of
        1 -> dti { currentHp = dti.currentHp - dmg }
        2 -> dti { currentHp = dti.currentHp - dmg }
        3 -> dti { currentHp = dti.currentHp - dmg }

calcNightHougeki :: NightBattle
                 -> AllFleetInfo DamageTookInfoNight
                 -> AllFleetInfo DamageTookInfoNight
calcNightHougeki nb xs = zipWith (map <<< combine) fleetDamages xs
  where
    h :: Hougeki
    h = nb.api_hougeki
    convert1 :: Foreign -> Array Int
    convert1 = unsafeFromForeign
    convert2 :: Foreign -> Array Number
    convert2 = unsafeFromForeign
    targets :: Array Int
    targets = concat (map convert1 (AU.tail h.api_df_list))
    damages :: Array Int
    damages = damageNormalize (concat (map convert2 (AU.tail h.api_damage)))
    -- summarize damage here
    fleetDamages :: Array Int
    fleetDamages = runEndo dmgModifiers $ replicate 13 0
      where
        dmgModifiers :: Endo (Array Int)
        dmgModifiers = mconcat $ zipWith combine targets damages
          where
            combine t d = Endo $ fromJust <<< modifyAt t (+ d)
    combine :: Int -> DamageTookInfoNight -> DamageTookInfoNight
    combine dmg dti = dti { currentHp = dti.currentHp - dmg }

calcRaigeki :: Raigeki
            -> Int -- 1 for opening, 2 for closing
            -> AllFleetInfo DamageTookInfo
            -> AllFleetInfo DamageTookInfo
calcRaigeki r i xs = zipWith (map <<< combine) damages xs
  where
    fDam = damageNormalize (AU.tail r.api_fdam)
    eDam = damageNormalize (AU.tail r.api_edam)
    damages = [-1] <> fDam <> eDam
    combine :: Int -> DamageTookInfo -> DamageTookInfo
    combine dmg dti = case i of
        1 -> dti { currentHp = dti.currentHp - dmg }
        2 -> dti { currentHp = dti.currentHp - dmg }

calcOpeningRaigeki :: DamageAnalyzer
calcOpeningRaigeki b xs | hasHourai b = if b.api_opening_flag == 1
  then calcRaigeki b.api_opening_atack 1 xs
  else xs
calcOpeningRaigeki _ xs = xs

calcClosingRaigeki :: DamageAnalyzer
calcClosingRaigeki b xs | hasHourai b = if AU.unsafeIndex b.api_hourai_flag 3 == 1
  then calcRaigeki b.api_raigeki 2 xs
  else xs
calcClosingRaigeki _ xs = xs

calcAllHougeki :: DamageAnalyzer
calcAllHougeki b | hasHourai b = runEndo (h1 <> h2 <> h3)
  where
    actionFlags = take 3 b.api_hourai_flag
    h1 :: Endo (AllFleetInfo DamageTookInfo)
    h1 = if AU.unsafeIndex actionFlags 0 == 1
           then Endo $ calcHougeki (b.api_hougeki1) 1
           else mempty
    h2 :: Endo (AllFleetInfo DamageTookInfo)
    h2 = if AU.unsafeIndex actionFlags 1 == 1
           then Endo $ calcHougeki (b.api_hougeki2) 2
           else mempty
    h3 :: Endo (AllFleetInfo DamageTookInfo)
    h3 = if AU.unsafeIndex actionFlags 2 == 1
           then Endo $ calcHougeki (b.api_hougeki3) 3
           else mempty
calcAllHougeki b = id

analyzeBattle :: Battle -> AllFleetInfo DamageTookInfo
analyzeBattle b = analyzeBattleAlt b
  where
    analyzers :: Endo (AllFleetInfo DamageTookInfo)
    analyzers = foldMap (Endo <<< ($ b))
                        [ calcAerial
                        , calcAerial2
                        , calcOpeningRaigeki
                        , calcAllHougeki
                        , calcClosingRaigeki
                        ]

analyzeNightBattle :: NightBattle -> AllFleetInfo DamageTookInfoNight
analyzeNightBattle b = calcNightHougeki b (battleStartNight b)

analyzeRawBattle :: Foreign -> AllFleetInfo DamageTookInfo
analyzeRawBattle = analyzeBattle <<< unsafeFromForeign

analyzeRawNightBattle :: Foreign -> AllFleetInfo DamageTookInfoNight
analyzeRawNightBattle = analyzeNightBattle <<< unsafeFromForeign

analyzeRawBattleJS :: Foreign -> Array (Nullable DamageTookInfo)
analyzeRawBattleJS = map toNullable <<< analyzeRawBattle

analyzeRawNightBattleJS :: Foreign -> Array (Nullable DamageTookInfoNight)
analyzeRawNightBattleJS = map toNullable <<< analyzeRawNightBattle

applyDamageVector :: DamageVector 
                  -> AllFleetInfo DamageTookInfo 
                  -> AllFleetInfo DamageTookInfo
applyDamageVector (DV dv) = zipWith combine dv
  where
    combine dmg = map (\x -> x {currentHp = x.currentHp - dmg})

analyzeBattleAlt :: Battle -> AllFleetInfo DamageTookInfo
analyzeBattleAlt = applyDamageVector <$> battleDV <*> battleStart
