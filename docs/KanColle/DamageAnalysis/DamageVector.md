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

#### `calcKoukuDamageCombined`

``` purescript
calcKoukuDamageCombined :: Kouku -> DamageVector
```

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

#### `calcSupportAirAttackDamage`

``` purescript
calcSupportAirAttackDamage :: SupportAirInfo -> DamageVector
```

#### `calcSupportHouraiDamage`

``` purescript
calcSupportHouraiDamage :: SupportHouraiInfo -> DamageVector
```

#### `FleetRole`

``` purescript
data FleetRole
  = FRMain
  | FREscort
  | FRSupport
```

#### `CombinedDamageVector`

``` purescript
newtype CombinedDamageVector
  = CDV { main :: DamageVector, escort :: DamageVector, support :: DamageVector }
```

##### Instances
``` purescript
instance combinedDamageVectorSemigroup :: Semigroup CombinedDamageVector
instance combinedDamageVectorMonoid :: Monoid CombinedDamageVector
```

#### `toCombined`

``` purescript
toCombined :: FleetRole -> DamageVector -> CombinedDamageVector
```


