## Module KanColle.Expedition.Base

#### `allExpeditionIds`

``` purescript
allExpeditionIds :: Array Int
```

#### `ResourceRows`

``` purescript
type ResourceRows a = { fuel :: a, ammo :: a, steel :: a, bauxite :: a }
```

`ResourceRows a` represents attributes of 4 resources.
All of the attributes have to be of the same type, namely `a`.

#### `resourceRowsFill`

``` purescript
resourceRowsFill :: forall a. a -> ResourceRows a
```

#### `resourceRowsLiftOp`

``` purescript
resourceRowsLiftOp :: forall a b. (a -> a -> b) -> ResourceRows a -> ResourceRows a -> ResourceRows b
```

#### `mapResourceRows`

``` purescript
mapResourceRows :: forall a b. (a -> b) -> ResourceRows a -> ResourceRows b
```


