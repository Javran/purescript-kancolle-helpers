module KanColle.Expedition.New.Resource where


import KanColle.Expedition.New.EArray
import KanColle.Expedition.New.Types

getResource :: Int -> Resource
getResource = indEA resources

-- | resource chart of the most basic income:
-- | normal success without daihatsu.
resources :: EArray Resource
resources = mkEA
    [ -- World 1 (1-8)
      i   0  30   0   0
    , i   0 100  30   0
    , i  30  30  40   0
    , i   0  60   0   0
    , i 200 200  20  20
    , i   0   0   0  80
    , i   0   0  50  30
    , i  50 100  50  50
      -- World 2 (9-16)
    , i 350   0   0   0
    , i   0  50   0  30
    , i   0   0   0 250
    , i  50 250 200  50
    , i 240 300   0   0
    , i   0 240 200   0
    , i   0   0 300 400
    , i 500 500 200 200
      -- World 3 (17-24)
    , i  70  70  50   0
    , i   0   0 300 100
    , i 400   0  50  30
    , i   0   0 150   0
    , i 320 270   0   0
    , i   0  10   0   0
    , i   0  20   0 100
    , i 500   0   0 150
      -- World 4 (25-32)
    , i 900   0 500   0
    , i   0   0   0 900
    , i   0   0 800   0
    , i   0   0 900 350
    , i   0   0   0 100
    , i   0   0   0 100
    , i   0  30   0   0
    , i  50  50  50  50
      -- World 5 (33-40)
    , i   0   0   0   0
    , i   0   0   0   0    
    , i   0   0 240 280
    , i 480   0 200 200
    , i   0 380 270   0
    , i 420   0 200   0
    , i   0   0 300   0
    , i 300 300   0 100
    ]
  where
    i f a s b = Rsc
        { fuel: f
        , ammo: a
        , steel: s
        , bauxite: b
        }
