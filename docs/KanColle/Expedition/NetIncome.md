## Module KanColle.Expedition.NetIncome

#### `NetIncome`

``` purescript
type NetIncome = { eId :: Int, netIncome :: Income }
```

#### `HourlyIncome`

``` purescript
type HourlyIncome = { fuel :: Number, ammo :: Number, steel :: Number, bauxite :: Number }
```

#### `ExpeNetIncomeHourly`

``` purescript
type ExpeNetIncomeHourly = { eId :: Int, hourly :: HourlyIncome }
```

#### `mergeHNetIncome`

``` purescript
mergeHNetIncome :: forall f. (Functor f, Foldable f) => f ExpeNetIncomeHourly -> { eIds :: Array Int, hourly :: HourlyIncome }
```

#### `netIncomeTable`

``` purescript
netIncomeTable :: Array NetIncome
```

#### `showNetIncome`

``` purescript
showNetIncome :: NetIncome -> String
```

#### `netIncomeWithAfkTime`

``` purescript
netIncomeWithAfkTime :: Int -> Array ExpeNetIncomeHourly
```

#### `showNetHourlyIncome`

``` purescript
showNetHourlyIncome :: ExpeNetIncomeHourly -> String
```


