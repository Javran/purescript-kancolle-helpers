## Module KanColle.DamageAnalysis.Stages

#### `koukuDV`

``` purescript
koukuDV :: Battle -> DamageVector
```

get `DamageVector` of kouku stage from battle data
all the names in this module are kept consistent with functions in
`KanColle.DamageAnalysis.DamageVector`.

#### `kouku2DV`

``` purescript
kouku2DV :: Battle -> DamageVector
```

#### `openingDV`

``` purescript
openingDV :: Battle -> DamageVector
```

#### `hougeki1DV`

``` purescript
hougeki1DV :: Battle -> DamageVector
```

#### `hougeki2DV`

``` purescript
hougeki2DV :: Battle -> DamageVector
```

#### `hougeki3DV`

``` purescript
hougeki3DV :: Battle -> DamageVector
```

#### `raigekiDV`

``` purescript
raigekiDV :: Battle -> DamageVector
```

#### `hougekiDV`

``` purescript
hougekiDV :: Battle -> DamageVector
```

#### `battleDV`

``` purescript
battleDV :: Battle -> DamageVector
```

get `DamageVector` of a regular / aerial battle from battle data
a regular battle consists of the following stages:

* `kouku`  (aerial battle)
* `kouku2` (aerial battle)
* `opening` (openning torpedo attack)
* `hougeki1` (first shelling stage)
* `hougeki2` (second shelling stage)
* `hougeki3` (third shelling stage, always empty for now)
* `raigeki` (closing torpedo attack)

#### `nightBattleDV`

``` purescript
nightBattleDV :: Battle -> DamageVector
```

get `DamageVector` of a night battle
a night battle involves only `hougeki` (shelling stage)


