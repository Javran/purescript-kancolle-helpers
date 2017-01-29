module KanColle.Expedition.New.MinCompo
 ( getMinimumComposition
 ) where

import Prelude
import Data.Maybe

import KanColle.Expedition.New.Types
import KanColle.Expedition.New.EArray

import Data.Unfoldable hiding (fromMaybe)
import Data.Array as A

getMinimumComposition :: Int -> MinFleetCompo
getMinimumComposition = indEA minimumCompositions

concretizeComposition :: Int -> SType -> MinFleetCompo -> FleetCompo
concretizeComposition expectCount wildcard mfc = 
    concretize <$> (atLeast expectCount mfc)
  where
    atLeast n compo = if A.length compo < n
        then compo <> replicate (n - A.length compo) Nothing
        else compo
    concretize = fromMaybe wildcard

-- | minimum compositions for all expeditions
minimumCompositions :: EArray MinFleetCompo
minimumCompositions = mkEA
    [ -- Exped 1
      atLeast 2 []
    , -- Exped 2
      atLeast 4 []
    , -- Exped 3
      atLeast 3 []
    , -- Exped 4
      sty 1 CL <> sty 2 DD
    , -- Exped 5
      atLeast 4 (sty 1 CL <> sty 2 DD)
    , -- Exped 6
      atLeast 4 []
    , -- Exped 7
      full []
    , -- Exped 8
      full []
      
    , -- Exped 9
      atLeast 4 (sty 1 CL <> sty 2 DD)
    , -- Exped 10
      atLeast 3 (sty 2 CL)
    , -- Exped 11
      atLeast 4 (sty 2 DD)
    , -- Exped 12
      atLeast 4 (sty 2 DD)
    , -- Exped 13
      full (sty 1 CL <> sty 4 DD)
    , -- Exped 14
      full (sty 1 CL <> sty 3 DD)
    , -- Exped 15
      full (sty 2 CVLike <> sty 2 DD)
    , -- Exped 16
      full (sty 1 CL <> sty 2 DD)
      
    , -- Exped 17
      full (sty 1 CL <> sty 3 DD)
    , -- Exped 18
      full (sty 3 CVLike <> sty 2 DD)
    , -- Exped 19
      full (sty 2 BBV <> sty 2 DD)
    , -- Exped 20
      sty 1 SSLike <> sty 1 CL
    , -- Exped 21
      sty 1 CL <> sty 4 DD
    , -- Exped 22
      full (sty 1 CA <> sty 1 CL <> sty 2 DD)
    , -- Exped 23
      full (sty 2 BBV <> sty 2 DD)
    , -- Exped 24
      full (sty 1 CL <> sty 4 DD)
    
    , -- Exped 25
      sty 2 CA <> sty 2 DD
    , -- Exped 26
      sty 1 CVLike <> sty 1 CL <> sty 2 DD
    , -- Exped 27
      sty 2 SSLike
    , -- Exped 28
      sty 3 SSLike
    , -- Exped 29
      sty 3 SSLike
    , -- Exped 30
      sty 4 SSLike
    , -- Exped 31
      sty 4 SSLike
    , -- Exped 32
      sty 1 CT <> sty 2 DD
     
    , -- Exped 33
      sty 2 DD 
    , -- Exped 34
      sty 2 DD
    , -- Exped 35
      full (sty 2 CVLike <> sty 1 CA <> sty 1 DD)
    , -- Exped 36
      full (sty 2 AV <> sty 1 CL <> sty 1 DD)
    , -- Exped 37
      sty 1 CL <> sty 5 DD
    , -- Exped 38
      full (sty 5 DD)
    , -- Exped 39
      sty 1 AS <> sty 4 SSLike
    , -- Exped 40
      full (sty 1 CL <> sty 2 AV <> sty 2 DD)
    ]
  where
    full = atLeast 6
    -- fill wildcards to a composition
    -- to meet the ship number requirement
    atLeast n compo = if A.length compo < n
        then compo <> replicate (n - A.length compo) Nothing
        else compo
    sty n st = replicate n (Just st)
