## Module KanColle.Expedition.IncomeBase

#### `IncomeBase`

``` purescript
newtype IncomeBase
```

`IncomeBase` corresponses to resource income found
on many KanColle wikis, neither great success nor
numbers of landing craft is taken into account.

##### Instances
``` purescript
Semigroup IncomeBase
Monoid IncomeBase
```

#### `getIncomeBase`

``` purescript
getIncomeBase :: IncomeBase -> ResourceRows Int
```

unwrap `IncomeBase` to expose its members

#### `mkIncomeBase`

``` purescript
mkIncomeBase :: ResourceRows Int -> IncomeBase
```

#### `getExpeditionIncomeBase`

``` purescript
getExpeditionIncomeBase :: Int -> IncomeBase
```

input a valid expedition id and get expedition income.


