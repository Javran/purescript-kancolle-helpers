## Module KanColle.DamageAnalysis.Damage

This module implements `Damage`:
a value that can be applied to a `Ship`
and `DameCon` is automatically consumed in the process if necessary.

#### `Damage`

``` purescript
newtype Damage
```

a `Damage` represents a damage value to be applied
to a `Ship`

##### Instances
``` purescript
Semigroup Damage
Monoid Damage
```

#### `damageToInt`

``` purescript
damageToInt :: Damage -> Int
```

converts `Damage` into the damage value

#### `mkDamage`

``` purescript
mkDamage :: Int -> Damage
```

create `Damage` from damage value

#### `mergeDamage`

``` purescript
mergeDamage :: Damage -> Damage -> Damage
```

create new `Damage` by applying two `Damage`s in order

#### `addDamage`

``` purescript
addDamage :: Damage -> Int -> Damage
```

add damage value to an existing `Damage`

#### `applyDamage`

``` purescript
applyDamage :: Damage -> Ship -> Ship
```

apply `Damage` to a `Ship`, DameCon will be used if applicable.


