## Module KanColle.DamageAnalysis.Types

#### `DameCon`

``` purescript
data DameCon
  = RepairTeam
  | RepairGoddess
```

##### Instances
``` purescript
Eq DameCon
Show DameCon
```

#### `Ship`

``` purescript
type Ship = { hp :: Int, fullHp :: Int, sunk :: Boolean, dameCon :: Maybe DameCon }
```

#### `mkShip`

``` purescript
mkShip :: Int -> Int -> Maybe DameCon -> Ship
```

#### `NormalBattle`

``` purescript
type NormalBattle a = { main :: a, enemy :: a }
```

#### `CombinedBattle`

``` purescript
type CombinedBattle a = { main :: a, escort :: a, enemy :: a }
```

#### `lrToNormal`

``` purescript
lrToNormal :: forall a. LR a -> NormalBattle a
```

#### `dupAsNormalBattle`

``` purescript
dupAsNormalBattle :: forall a. a -> NormalBattle a
```

#### `appNormalBattle`

``` purescript
appNormalBattle :: forall a b. NormalBattle (a -> b) -> NormalBattle a -> NormalBattle b
```

#### `dupAsCombinedBattle`

``` purescript
dupAsCombinedBattle :: forall a. a -> CombinedBattle a
```

#### `appCombinedBattle`

``` purescript
appCombinedBattle :: forall a b. CombinedBattle (a -> b) -> CombinedBattle a -> CombinedBattle b
```

#### `mapNormalBattle`

``` purescript
mapNormalBattle :: forall a b. (a -> b) -> NormalBattle a -> NormalBattle b
```

#### `mapCombinedBattle`

``` purescript
mapCombinedBattle :: forall a b. (a -> b) -> CombinedBattle a -> CombinedBattle b
```

#### `ShipResult`

``` purescript
type ShipResult = { hp :: Int, sunk :: Boolean, dameConConsumed :: Boolean }
```

#### `getShipResult`

``` purescript
getShipResult :: Ship -> Ship -> ShipResult
```

#### `FleetInfo`

``` purescript
type FleetInfo a = Array (Maybe a)
```

#### `NormalFleetInfo`

``` purescript
type NormalFleetInfo a = NormalBattle (FleetInfo a)
```

#### `CombinedFleetInfo`

``` purescript
type CombinedFleetInfo a = CombinedBattle (FleetInfo a)
```


