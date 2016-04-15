## Module KanColle.Expedition.GrossIncome

#### `IncomeFactor`

``` purescript
type IncomeFactor = { greatSuccess :: Boolean, landingCraft :: Int }
```

`IncomeFactor` keeps track of factors that would
affect expedition income

#### `GrossIncome`

``` purescript
newtype GrossIncome
```

#### `getGrossIncome`

``` purescript
getGrossIncome :: GrossIncome -> ResourceRows Int
```

access gross income fields

#### `withFactorToGrossIncome`

``` purescript
withFactorToGrossIncome :: IncomeFactor -> IncomeBase -> GrossIncome
```

calculate gross income based on `IncomeBase`
together with some factors

#### `toGrossIncome`

``` purescript
toGrossIncome :: IncomeBase -> GrossIncome
```

calculate gross income based on `IncomeBase`
without extra factors

#### `showGrossIncome`

``` purescript
showGrossIncome :: GrossIncome -> String
```

visualize `GrossIncome` as a `String`


