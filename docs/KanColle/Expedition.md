## Module KanColle.Expedition

#### `validExpeditionId`

``` purescript
validExpeditionId :: Int -> Boolean
```

#### `getAvailableExpeditions`

``` purescript
getAvailableExpeditions :: forall a. Fleet a -> Array Int
```

#### `checkExpedition`

``` purescript
checkExpedition :: forall a. Int -> Fleet a -> Boolean
```

#### `Expedition`

``` purescript
type Expedition = { id :: Int, req :: ExpeditionRequirement, income :: IncomeBase, cost :: Cost }
```

#### `getExpeditionInfo`

``` purescript
getExpeditionInfo :: Int -> Expedition
```

#### `allExpeditions`

``` purescript
allExpeditions :: Array Expedition
```


