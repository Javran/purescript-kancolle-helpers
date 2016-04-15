## Module KanColle.Expedition.Plan

#### `calcNetIncome`

``` purescript
calcNetIncome :: Number -> Number -> Number -> Number -> Number -> Int -> Array { eIds :: Array Int, hourly :: HourlyIncome, resourceScore :: Number }
```

#### `calcNetIncomeWithFleetCount`

``` purescript
calcNetIncomeWithFleetCount :: Int -> Number -> Number -> Number -> Number -> Number -> Int -> Array { eIds :: Array Int, hourly :: HourlyIncome, resourceScore :: Number }
```

#### `PlanEvaluation`

``` purescript
type PlanEvaluation = { eIds :: Array Int, hourly :: HourlyIncome, resourceScore :: Number }
```

#### `showNI`

``` purescript
showNI :: { eIds :: Array Int, hourly :: HourlyIncome, resourceScore :: Number } -> String
```

#### `dbg`

``` purescript
dbg :: Number -> Number -> Number -> Number -> Int -> Number -> Eff (console :: CONSOLE) Unit
```

#### `quickCalc`

``` purescript
quickCalc :: Number -> Number -> Number -> Number -> Int -> Array { eIds :: Array Int, hourly :: HourlyIncome, resourceScore :: Number }
```

#### `quickCalcJS`

``` purescript
quickCalcJS :: Fn5 Number Number Number Number Int (Array { eIds :: Array Int, hourly :: HourlyIncome, resourceScore :: Number })
```

#### `calcWithExpeditionIds`

``` purescript
calcWithExpeditionIds :: Number -> Number -> Number -> Number -> Int -> Array Int -> Array PlanEvaluation
```

#### `calcWithExpeditionIdsFleetCount`

``` purescript
calcWithExpeditionIdsFleetCount :: Int -> Number -> Number -> Number -> Number -> Int -> Array Int -> Array PlanEvaluation
```

#### `calcWithExpeditionIdsJS`

``` purescript
calcWithExpeditionIdsJS :: Fn6 Number Number Number Number Int (Array Int) (Array PlanEvaluation)
```

#### `calcWithExpeditionIdsFleetCountJS`

``` purescript
calcWithExpeditionIdsFleetCountJS :: Fn7 Int Number Number Number Number Int (Array Int) (Array PlanEvaluation)
```


