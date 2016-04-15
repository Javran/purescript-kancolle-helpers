## Module KanColle.DamageAnalysis.Stages

This module implements attacking stages of battles
which accumulates damages values in the correct order
to construct `DamageVector`s for all damage-taking fleets involved in
the battle.

#### `koukuDV`

``` purescript
koukuDV :: Battle -> LR DamageVector
```

get `DamageVector` of kouku stage from battle data
all the names in this module are kept consistent with functions in
`KanColle.DamageAnalysis.DamageVector`.

#### `koukuCombinedDV`

``` purescript
koukuCombinedDV :: Battle -> DamageVector
```

#### `supportAirAttackDV`

``` purescript
supportAirAttackDV :: Battle -> DamageVector
```

#### `supportHouraiDV`

``` purescript
supportHouraiDV :: Battle -> DamageVector
```

#### `battleDV`

``` purescript
battleDV :: Battle -> NormalDamageVector
```

get `NormalDamageVector` of a regular / aerial battle from battle data
a regular battle consists of the following stages:

* `kouku`  (aerial battle)
* `kouku2` (aerial battle)
* `supportAirInfo` (airstrike from support expedition)
* `supportHouraiInfo` (shelling attack from support expedition)
* `opening` (openning torpedo attack)
* `hougeki1` (first shelling stage)
* `hougeki2` (second shelling stage)
* `hougeki3` (third shelling stage, always empty for regular battles)
* `raigeki` (closing torpedo attack)

#### `nightBattleDV`

``` purescript
nightBattleDV :: Battle -> NormalDamageVector
```

get `NormalDamageVector` of a night battle
a night battle involves only `hougeki` (shelling stage)

#### `battleSurfaceTaskForceDV`

``` purescript
battleSurfaceTaskForceDV :: Battle -> CombinedDamageVector
```

get `CombinedDamageVector` of a surface task force battle

#### `battleCarrierTaskForceDV`

``` purescript
battleCarrierTaskForceDV :: Battle -> CombinedDamageVector
```

get `CombinedDamageVector` of a carrier task force battle
note that transport escort battle uses this `CombinedDamageVector` as well


