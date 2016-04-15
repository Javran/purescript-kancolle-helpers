## Module KanColle.DamageAnalysis.DamageVector

This module implements `DamageVector`:
an array of `Damage`s that can be applied to one fleet of `Ship`s.
most functions prefixed with `calc` in this module
produces either `LR DamageVector` or a single `DamageVector`.
which can then be converted `NormalDamageVector` or `CombinedDamageVector`
depending on what the corresponding data is standing for.

#### `DamageVector`

``` purescript
newtype DamageVector
```

`DamageVector` is a 6-element array of `Damage`s

##### Instances
``` purescript
Semigroup DamageVector
Monoid DamageVector
```

#### `getDV`

``` purescript
getDV :: DamageVector -> Array Damage
```

fetches the array inside `DamageVector`

#### `mkDV`

``` purescript
mkDV :: Array Damage -> DamageVector
```

creates `DamageVector` from arrays.
array has to be of length 6

#### `NormalDamageVector`

``` purescript
type NormalDamageVector = NormalBattle DamageVector
```

`DamageVector` for normal battles

#### `CombinedDamageVector`

``` purescript
type CombinedDamageVector = CombinedBattle DamageVector
```

`DamageVector` for battles involving comined fleets

#### `calcKoukuDamage`

``` purescript
calcKoukuDamage :: Kouku -> LR DamageVector
```

calculate damage from kouku (aerial) stages

#### `calcKoukuDamageCombined`

``` purescript
calcKoukuDamageCombined :: Kouku -> DamageVector
```

calculate damage from kouku (aerial) stages (combined fleet)
note that only escort fleet is taking damage. so we just need DamageVector

#### `calcRaigekiDamage`

``` purescript
calcRaigekiDamage :: Raigeki -> LR DamageVector
```

calculate damage from raigeki (torpedo) stages

#### `calcHougekiDamage`

``` purescript
calcHougekiDamage :: Hougeki -> LR DamageVector
```

calculate damage from hougeki (shelling) stages

#### `calcSupportAirAttackDamage`

``` purescript
calcSupportAirAttackDamage :: SupportAirInfo -> DamageVector
```

calculate damage from support airstrike stages.
note that only enemy is taking damage so this results in
a single `DamageVector`.

#### `calcSupportHouraiDamage`

``` purescript
calcSupportHouraiDamage :: SupportHouraiInfo -> DamageVector
```

calculate damage from support shelling stages.
note that only enemy is taking damage so this results in
a single `DamageVector`.

#### `FleetRole`

``` purescript
data FleetRole
  = FRMain
  | FREscort
  | FRSupport
```

ally fleet's role in this battle

#### `toCombined`

``` purescript
toCombined :: FleetRole -> LR DamageVector -> CombinedDamageVector
```

`toCombined role dv` converts a `LR DamageVector` whose left part
is playing role `role` into `CombinedDamageVector`

#### `applyDamageVector`

``` purescript
applyDamageVector :: DamageVector -> FleetInfo Ship -> FleetInfo Ship
```

apply a single `DamageVector` on a single fleet

#### `applyNormalDamageVector`

``` purescript
applyNormalDamageVector :: NormalDamageVector -> NormalFleetInfo Ship -> NormalFleetInfo Ship
```

apply `NormalDamageVector` on a normal fleet of ships (including enemy ships)

#### `applyCombinedDamageVector`

``` purescript
applyCombinedDamageVector :: CombinedDamageVector -> CombinedFleetInfo Ship -> CombinedFleetInfo Ship
```

apply `CombinedDamageVector` on a combined fleet of ships (including enemy ships)


