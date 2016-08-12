module KanColle.Expedition.New.CostModel where

import Prelude
import KanColle.Expedition.New.SType
import Data.Maybe
import Control.MonadPlus

data MaxCost = MC
  { fuel :: Int
  , ammo :: Int
  }

instance semigroupMaxCost :: Semigroup MaxCost where
  append (MC a) (MC b) = MC {fuel: a.fuel, ammo: b.fuel}

mkMC :: Int -> Int -> MaxCost
mkMC fuel ammo = MC {fuel: fuel, ammo: ammo}

-- | multiple `MaxCost` with an integer
mcTimes :: MaxCost -> Int -> MaxCost
mcTimes _ 0 = mkMC 0 0
mcTimes (MC a) n = mkMC (a.fuel*n) (a.ammo*n)

-- | `CostModel` calculates the total maximum cost
-- | given a ship type and how many ships are present.
-- | This cost model is designed for just one fleet so
-- | the input number should only be one of `[0,1,2,3,4,5,6]`.
-- | Note that for a cost model `f`, it's not guaranteed
-- | that `f stype a + f stype b` and `f stype (a+b)` give
-- | the same answer.
type CostModel = SType -> Int -> Maybe MaxCost

-- | a normal cost model assumes all ships are remodelled to
-- | their final forms. (the exceptions are like Taigei, Chitose-A and Chiyoda-A
-- | as their further remodels also change ship type.)
normalCostModel :: CostModel
normalCostModel stype = case stype of
    DD -> ddCost
    CL -> clCost
    CVLLike -> cvlLikeCost
    SSLike -> ssLikeCost
    CA -> caCost
    BBV -> bbvCost
    AS -> asCost
    CT -> ctCost
    AV -> avCost
  where
    ddCost n =
      -- use Mutsuki class destroyers
      Just (mcTimes (mkMC 15 15) n)
    clCost n
      | n <= 2 =
        -- use 2 Tenryuu class ships,
        Just (mcTimes (mkMC 25 25) n)
      | n <= 6 = do
        -- if we need more, fill the rest with some Nagara class ships
        c <- clCost 2
        Just (c <> mcTimes (mkMC 25 30) (n-2))
      | otherwise = Nothing
    avCost n
      | n <= 2 =
        -- Chitose-A and Chiyoda-A
        Just (mcTimes (mkMC 35 45) n)
      | n <= 3 = do
        -- and Mizuho Kai
        c <- avCost 2
        Just (c <> mkMC 40 45)
      | n <= 4 = do
        -- and Akitsushima Kai
        c <- avCost 3
        Just (c <> mkMC 60 15)
      | otherwise = Nothing
    -- Taigei
    asCost n = do
        guard (n <= 1)
        Just (mcTimes (mkMC 35 10) n)
    -- Katori class
    ctCost n = do
        guard (n <= 2)
        Just (mcTimes (mkMC 35 20) n)
    caCost n
      | n <= 1 =
        -- Aoba Kai
        Just (mcTimes (mkMC 35 55) n)
      | n <= 4 = do
        -- Furutaka Kai Ni, Kako Kai Ni, Kinugasa Kai Ni
        c <- caCost 1
        Just (c <> mcTimes (mkMC 35 65) (n-1))
      | n <= 6 = do
        -- Takao Kai, Atago Kai
        c <- caCost 4
        Just (c <> mcTimes (mkMC 40 70) (n-4))
      | otherwise = Nothing
    bbvCost n
      | n <= 2 =
        -- Ise class
        Just (mcTimes (mkMC 95 105) n)
      | n <= 4 = do
        -- Fusou class
        c <- bbvCost 2
        Just (c <> mcTimes (mkMC 105 140) (n-2))
      | otherwise = Nothing
    ssLikeCost n
      | n <= 1 =
        -- Maruyu Kai
        Just (mcTimes (mkMC 10 10) n)
      | n <= 6 = do
        -- no need for SSV if we just consider:
        -- I-168, I-58, I-19, I-8 and Ro-500
        c <- ssLikeCost 1
        Just (mcTimes (mkMC 10 20) (n-1))
      | otherwise = Nothing
    cvlLikeCost n
      | n <= 1 =
        -- Houshou Kai
        Just (mcTimes (mkMC 30 30) n)
      | n <= 2 = do
        -- Ryuuhou Kai
        c <- cvlLikeCost 1
        Just (c <> mcTimes (mkMC 35 40) (n-1))
      | n <= 4 = do
        -- Chitose-A, Chiyoda-A
        c <- cvlLikeCost 2
        Just (c <> mcTimes (mkMC 35 45) (n-2))
      | n <= 6 = do
        -- Shouhou Kai, Zuihou Kai
        c <- cvlLikeCost 4
        Just (c <> mcTimes (mkMC 40 40) (n-4))
      | otherwise = Nothing
