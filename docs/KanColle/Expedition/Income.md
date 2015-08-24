## Module KanColle.Expedition.Income

#### `Income`

``` purescript
newtype Income
  = Income { fuel :: Int, ammo :: Int, steel :: Int, bauxite :: Int }
```

`Income` includes `fuel`, `ammo` `steel` and `bauxite` resource income.

##### Instances
``` purescript
instance incomeSemigroup :: Semigroup Income
instance incomeMonoid :: Monoid Income
```

#### `getIncome`

``` purescript
getIncome :: Income -> { fuel :: Int, ammo :: Int, steel :: Int, bauxite :: Int }
```

unwrap `Income` to expose its members

#### `getExpeditionIncome`

``` purescript
getExpeditionIncome :: Int -> Income
```

input a valid expedition id and get expedition income.


