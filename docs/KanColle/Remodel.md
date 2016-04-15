## Module KanColle.Remodel

#### `RemodelInfo`

``` purescript
type RemodelInfo = { shipIdFrom :: Int, shipIdTo :: Int, level :: Int, steel :: Int, ammo :: Int, catapult :: Int, blueprint :: Int, devmat :: Int }
```

#### `RemodelInfoMap`

``` purescript
type RemodelInfoMap = StrMap RemodelInfo
```

#### `RemodelGroup`

``` purescript
type RemodelGroup = { origin :: Int, finalForms :: Array Int, group :: Array Int }
```

#### `RemodelGroupMap`

``` purescript
type RemodelGroupMap = StrMap RemodelGroup
```

#### `OriginMap`

``` purescript
type OriginMap = StrMap Int
```

#### `RemodelDb`

``` purescript
type RemodelDb = { remodelInfo :: RemodelInfoMap, remodelGroups :: RemodelGroupMap, origins :: OriginMap, shipCount :: Int, upgradeCount :: Int }
```

#### `optTrace`

``` purescript
optTrace :: forall a. String -> (Unit -> a) -> a
```

#### `calcDevMat`

``` purescript
calcDevMat :: Int -> Int
```

#### `fromMstShip`

``` purescript
fromMstShip :: MstShip -> Maybe RemodelInfo
```

#### `maybeToArray`

``` purescript
maybeToArray :: forall a. Maybe a -> Array a
```

#### `collectRemodelInfo1`

``` purescript
collectRemodelInfo1 :: Array MstShip -> RemodelInfoMap
```

#### `collectRemodelInfo2`

``` purescript
collectRemodelInfo2 :: Array MstShipUpgrade -> RemodelInfoMap -> RemodelInfoMap
```

#### `collectRemodelInfo`

``` purescript
collectRemodelInfo :: Master -> RemodelInfoMap
```

#### `generateRemodelGroups`

``` purescript
generateRemodelGroups :: RemodelInfoMap -> RemodelGroupMap
```

#### `generateRemodelGroup`

``` purescript
generateRemodelGroup :: RemodelInfoMap -> Int -> RemodelGroup
```

#### `listToArray`

``` purescript
listToArray :: forall a. List a -> Array a
```

#### `generateOriginMap`

``` purescript
generateOriginMap :: RemodelGroupMap -> OriginMap
```

#### `generateRemodelDb`

``` purescript
generateRemodelDb :: Master -> RemodelDb
```


