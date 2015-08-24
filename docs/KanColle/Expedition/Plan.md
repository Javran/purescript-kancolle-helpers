## Module KanColle.Expedition.Plan

#### `tails`

``` purescript
tails :: forall a. List a -> List (List a)
```

#### `chooseN`

``` purescript
chooseN :: forall a f. (Foldable f) => f a -> Int -> Array (List a)
```

#### `calcNetIncome`

``` purescript
calcNetIncome :: Number -> Number -> Number -> Number -> Number -> Int -> Array { eIds :: Array Int, hourly :: HourlyIncome, resourceScore :: Number }
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

#### `ratioPenalty`

``` purescript
ratioPenalty :: Array Number -> Array Number -> Number
```

#### `calcWithExpeditionIds`

``` purescript
calcWithExpeditionIds :: Number -> Number -> Number -> Number -> Int -> Array Int -> Array PlanEvaluation
```

#### `calcWithExpeditionIdsJS`

``` purescript
calcWithExpeditionIdsJS :: Fn6 Number Number Number Number Int (Array Int) (Array PlanEvaluation)
```


