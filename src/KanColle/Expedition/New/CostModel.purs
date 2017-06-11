module KanColle.Expedition.New.CostModel
  ( calcFleetMaxCost
  , calcActualCost
  , calcFleetActualCost
  , normalCostModel
  ) where

import Prelude
import KanColle.Expedition.New.Types
import Data.Map as M
import Data.Array as A
import Data.Maybe
import Control.MonadPlus
import Data.Unfoldable
import Data.Tuple
import Data.Traversable
import Data.Int
import Data.List as L

calcFleetMaxCost :: CostModel -> FleetCompo -> Maybe FleetMaxCost
calcFleetMaxCost cm fcAr = fold <$> sequence compos
  where
    fc :: M.Map SType Int
    fc = M.fromFoldableWith (+) (map (\x -> Tuple x 1) fcAr)
    compos = map (\(Tuple sty cnt) -> cm sty cnt) (M.toUnfoldable fc :: L.List _)

calcActualCost :: MaxCost -> Info -> ActualCost
calcActualCost (MCost mc) info = ACost
    { fuel: floor (info.fuelCostPercent * toNumber mc.fuel)
    , ammo: floor (info.ammoCostPercent * toNumber mc.ammo)
    }

calcFleetActualCost :: FleetMaxCost -> Info -> FleetActualCost
calcFleetActualCost fmc info = FACost (foldl merge z fmc)
  where
    z = {fuel: 0, ammo: 0}
    merge acc mc = case calcActualCost mc info of
      (ACost actual) ->
        {fuel: acc.fuel+actual.fuel, ammo: acc.ammo+actual.ammo}

-- | take some elements from a list, fail if the array does not have sufficient elements        
takeOrFail :: forall a. Int -> Array a -> Maybe (Array a)
takeOrFail n xs
  | n <= A.length xs = Just (A.take n xs)
  | otherwise = Nothing

-- | a normal cost model assumes all ships are remodelled to
-- | their final forms. (the exceptions are like Taigei, Chitose-A and Chiyoda-A
-- | as their further remodels also change ship type.)
normalCostModel :: CostModel
normalCostModel stype = case stype of
    DD -> ddCost
    CL -> clCost
    CVLike -> cvlLikeCost
    SSLike -> ssLikeCost
    CA -> caCost
    BBV -> bbvCost
    AS -> asCost
    CT -> ctCost
    AV -> avCost
  where
    ddCost n =
      -- use Mutsuki class destroyers
      Just (replicate n (mkMC 15 15))
    clCost n
      | n <= 2 =
        -- use 2 Tenryuu class ships,
        Just (replicate n (mkMC 25 25))
      | n <= 6 = do
        -- if we need more, fill the rest with some Nagara class ships
        c <- clCost 2
        Just (c <> replicate (n-2) (mkMC 25 30))
      | otherwise = Nothing
    avCost n
      | n <= 2 =
        -- Chitose-A and Chiyoda-A
        Just (replicate n (mkMC 35 45))
      | n <= 3 = do
        -- and Mizuho Kai
        c <- avCost 2
        Just (c <> [mkMC 40 45])
      | n <= 4 = do
        -- and Commandant Teste Kai      
        c <- avCost 3
        Just (c <> [mkMC 40 50])        
      | n <= 5 = do
        -- and Akitsushima Kai
        c <- avCost 4
        Just (c <> [mkMC 60 15])        
      | otherwise = Nothing
    -- Taigei
    asCost n = do
        guard (n <= 1)
        Just (replicate n (mkMC 35 10))
    -- Katori class
    ctCost n = do
        guard (n <= 2)
        Just (replicate n (mkMC 35 20))
    caCost n
      | n <= 1 =
        -- Aoba Kai
        Just (replicate n (mkMC 35 55))
      | n <= 4 = do
        -- Furutaka Kai Ni, Kako Kai Ni, Kinugasa Kai Ni
        c <- caCost 1
        Just (c <> replicate (n-1) (mkMC 35 65))
      | n <= 6 = do
        -- Takao Kai, Atago Kai
        c <- caCost 4
        Just (c <> replicate (n-4) (mkMC 40 70))
      | otherwise = Nothing
    bbvCost n
      | n <= 2 =
        -- Ise class
        Just (replicate n (mkMC 95 105))
      | n <= 4 = do
        -- Fusou class
        c <- bbvCost 2
        Just (c <> replicate (n-2) (mkMC 105 140))
      | otherwise = Nothing
    ssLikeCost n
      | n <= 1 =
        -- Maruyu Kai
        Just (replicate n (mkMC 10 10))
      | n <= 6 = do
        -- no need for SSV if we just consider:
        -- I-168, I-58, I-19, I-8 and Ro-500
        c <- ssLikeCost 1
        Just (c <> replicate (n-1) (mkMC 10 20))
      | otherwise = Nothing
    cvlLikeCost n
      | n <= 1 =
        -- Houshou Kai
        Just (replicate n (mkMC 30 30))
      | n <= 2 = do
        -- Ryuuhou Kai
        c <- cvlLikeCost 1
        Just (c <> replicate (n-1) (mkMC 35 40))
      | n <= 4 = do
        -- Chitose-A, Chiyoda-A
        c <- cvlLikeCost 2
        Just (c <> replicate (n-2) (mkMC 35 45))
      | n <= 6 = do
        -- Shouhou Kai, Zuihou Kai
        c <- cvlLikeCost 4
        Just (c <> replicate (n-4) (mkMC 40 40))
      | otherwise = Nothing
