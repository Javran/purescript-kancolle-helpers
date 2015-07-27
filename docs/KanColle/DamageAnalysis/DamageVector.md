## Module KanColle.DamageAnalysis.DamageVector

#### `DamageVector`

``` purescript
newtype DamageVector
  = DV (Array Int)
```

`DamageVector` is an array of integers that a value `x :: Array Int`
 satisfies the following properties
* `length x == 13`
* `head x == Just 0`
* `all (>= 0) x`

##### Instances
``` purescript
instance damageVectorSemigroup :: Semigroup DamageVector
instance damageVectorMonoid :: Monoid DamageVector
instance damageVectorShow :: Show DamageVector
```

#### `calcKoukuDamage`

``` purescript
calcKoukuDamage :: Kouku -> DamageVector
```

calculate damage from kouku (aerial) stages

#### `calcHougekiDamage`

``` purescript
calcHougekiDamage :: Hougeki -> DamageVector
```

calculate damage from hougeki (shelling) stages

#### `calcRaigekiDamage`

``` purescript
calcRaigekiDamage :: Raigeki -> DamageVector
```

calculate damage from raigeki (torpedo) stages


