module KanColle.Remodel where

import Prelude
import Data.Maybe
import Data.Array hiding (length)
import Data.Int as I
import Data.Foldable
import KanColle.KCAPI.Master
import Data.StrMap as SM
import Data.List as L
import Data.List.Partial as PL
import Partial.Unsafe
import Data.Set as S
import Data.Tuple
import Debug.Trace

-- things related to ship remodeling
type RemodelInfo =
  { shipIdFrom :: Int
  , shipIdTo :: Int
  , level :: Int
  , steel :: Int
  , ammo :: Int
  , catapult :: Int
  , blueprint :: Int
  , devmat :: Int
  }
  
type RemodelInfoMap = SM.StrMap RemodelInfo

type RemodelGroup =
  { origin :: Int -- origin ship id
  , finalForms :: Array Int -- all final forms
  , group :: Array Int -- ships that have same origin (including origin herself)
  }
  
type RemodelGroupMap = SM.StrMap RemodelGroup

type OriginMap = SM.StrMap Int

type RemodelDb =
  { remodelInfo :: RemodelInfoMap
  , remodelGroups :: RemodelGroupMap
  , origins :: OriginMap
  , shipCount :: Int
  , upgradeCount :: Int
  }

-- print only when the string is not empty
optTrace :: forall a. String -> (Unit -> a) -> a
optTrace s thunk
    | s == "" = thunk unit
    | otherwise = trace s thunk

-- get amount of DevMat required from steel
calcDevMat :: Int -> Int
calcDevMat steel
  | steel < 4500 = 0
  | steel < 5500 = 10
  | steel < 6500 = 15
  | otherwise = 20

-- catapult and blueprint info is missing in the result.
fromMstShip :: MstShip -> Maybe RemodelInfo
fromMstShip ms
  | ms.api_id >= 500 || ms.api_aftershipid == "0" = Nothing
  | otherwise =
    let result =
          { shipIdFrom: ms.api_id
          , shipIdTo: unsafePartial fromJust (I.fromString ms.api_aftershipid)
          , level: ms.api_afterlv
          , steel: ms.api_afterfuel
          , ammo: ms.api_afterbull
          , catapult: 0
          , blueprint: 0
          , devmat: 0
          }
  in Just (result {devmat = calcDevMat result.steel})
  
maybeToArray :: forall a. Maybe a -> Array a
maybeToArray (Just x) = [x]
maybeToArray Nothing = []

collectRemodelInfo1 :: Array MstShip -> RemodelInfoMap
collectRemodelInfo1 mstShips = SM.fromFoldable (map mkTup remodelInfoAr)
  where
    remodelInfoAr = foldMap (fromMstShip >>> maybeToArray) mstShips
    mkTup x = Tuple (show x.shipIdFrom) x

collectRemodelInfo2 :: Array MstShipUpgrade -> RemodelInfoMap -> RemodelInfoMap
collectRemodelInfo2 mstShipUpgrades rim = foldl combine rim upgrades
  where
    upgrades = filter (\x -> x.api_current_ship_id /= 0) mstShipUpgrades
    combine rim' upgrade = SM.alter (map modifyRemodel) sidStr rim'
      where
        sidStr = show upgrade.api_current_ship_id
        modifyRemodel :: RemodelInfo -> RemodelInfo
        modifyRemodel ri = optTrace check (\_ -> ri
            { catapult=upgrade.api_catapult_count
            , blueprint=upgrade.api_drawing_count
            })
          where
            -- check and print messages if there's any possible error
            check
              | ri.shipIdTo == upgrade.api_id = ""
              | otherwise = "WARNING: data inconsistent for id: " <> show ri.shipIdFrom

collectRemodelInfo :: Master -> RemodelInfoMap
collectRemodelInfo mst = result2
  where
    result1 = collectRemodelInfo1 mst.api_mst_ship
    result2 = collectRemodelInfo2 mst.api_mst_shipupgrade result1

generateRemodelGroups :: RemodelInfoMap -> RemodelGroupMap
generateRemodelGroups rim = SM.fromFoldable (map build originIds)
  where
    infoList = SM.values rim
    srcDstSets = foldl go (Tuple S.empty S.empty) infoList
    go tp ri = Tuple (S.insert ri.shipIdFrom srcSet) (S.insert ri.shipIdTo dstSet)
      where
        srcSet = fst tp
        dstSet = snd tp
    originIds = S.toUnfoldable (uncurry S.difference srcDstSets) :: L.List Int
    build oId = Tuple (show oId) (generateRemodelGroup rim oId)

generateRemodelGroup :: RemodelInfoMap -> Int -> RemodelGroup
generateRemodelGroup rim originId =
    { origin: originId
    , finalForms: finalForms
    , group: group }
  where
    getRI i = SM.lookup (show i) rim
    ri = unsafePartial fromJust (getRI originId)
    findLoop :: L.List Int -> Int -> L.List Int
    findLoop visited cur = case L.elemIndex cur visited of
        Nothing ->
          let newVisited = L.Cons cur visited
          in case getRI cur of
            Nothing -> newVisited
            Just next -> findLoop newVisited next.shipIdTo
        Just _ -> visited
    -- reversed
    groupL' = findLoop L.Nil originId
    group = reverse (listToArray groupL')
    finalFormsL' = findLoop L.Nil ((unsafePartial PL.head) groupL')
    finalForms = reverse (listToArray finalFormsL')

listToArray :: forall a. L.List a -> Array a
listToArray = L.toUnfoldable

generateOriginMap :: RemodelGroupMap -> OriginMap
generateOriginMap rgm =
    SM.fromFoldable (foldMap expand (SM.toUnfoldable rgm :: L.List (Tuple String RemodelGroup)))
  where
    expand (Tuple _ rg) = map (\x -> Tuple (show x) rg.origin) rg.group

generateRemodelDb :: Master -> RemodelDb
generateRemodelDb mst = 
    { remodelInfo: rim
    , remodelGroups: rgs
    , origins: ogs
    , shipCount: length mst.api_mst_ship
    , upgradeCount: length mst.api_mst_shipupgrade
    }
  where
    rim = collectRemodelInfo mst
    rgs = generateRemodelGroups rim
    ogs = generateOriginMap rgs
