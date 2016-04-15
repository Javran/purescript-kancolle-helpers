## Module KanColle.Expedition.Evaluate

#### `EvalResult`

``` purescript
type EvalResult = { eId :: Int, netIncome :: IncomeBase, score :: Number, time :: Int }
```

#### `nToFloor`

``` purescript
nToFloor :: Number -> Int
```

#### `incomeDiff`

``` purescript
incomeDiff :: Cost -> IncomeBase -> ECost -> IncomeBase
```

#### `ordMax`

``` purescript
ordMax :: forall a. (Ord a) => a -> a -> a
```

#### `minToHour`

``` purescript
minToHour :: Int -> Number
```

#### `comparing`

``` purescript
comparing :: forall a b. (Ord a) => (b -> a) -> b -> b -> Ordering
```

#### `sortByHourlyGain`

``` purescript
sortByHourlyGain :: (IncomeBase -> Int) -> Array EvalResult
```

#### `sortWithAfkTime`

``` purescript
sortWithAfkTime :: (IncomeBase -> Int) -> Number -> Int -> Array EvalResult
```

#### `showEvalResult`

``` purescript
showEvalResult :: EvalResult -> String
```

#### `evalResultToJS`

``` purescript
evalResultToJS :: EvalResult -> { eId :: Int, result :: Array Number }
```

#### `simpleEvalCost`

``` purescript
simpleEvalCost :: (Int -> Int -> Int -> Int -> Int) -> IncomeBase -> Int
```

#### `evalNetIncomeHourlyJS`

``` purescript
evalNetIncomeHourlyJS :: Fn4 Int Int Int Int Int -> Array { eId :: Int, result :: Array Number }
```

#### `evalNetIncomeWithAfkMinutesJS'`

``` purescript
evalNetIncomeWithAfkMinutesJS' :: Fn4 Int Int Int Int Int -> Number -> Int -> Array { eId :: Int, result :: Array Number }
```

#### `evalNetIncomeWithAfkMinutesJS`

``` purescript
evalNetIncomeWithAfkMinutesJS :: Fn3 (Fn4 Int Int Int Int Int) Number Int (Array { eId :: Int, result :: Array Number })
```


