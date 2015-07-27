## Module KanColle.DamageAnalysis

#### `DamageTookInfo`

``` purescript
type DamageTookInfo = { currentHp :: Int }
```

#### `DamageTookInfoNight`

``` purescript
type DamageTookInfoNight = { currentHp :: Int }
```

#### `pprDamageTookInfo`

``` purescript
pprDamageTookInfo :: DamageTookInfo -> String
```

#### `pprDamageTookInfoNight`

``` purescript
pprDamageTookInfoNight :: DamageTookInfoNight -> String
```

#### `pprFleetDamageTookInfo`

``` purescript
pprFleetDamageTookInfo :: AllFleetInfo DamageTookInfo -> String
```

#### `pprFleetDamageTookInfoNight`

``` purescript
pprFleetDamageTookInfoNight :: AllFleetInfo DamageTookInfoNight -> String
```

#### `AllFleetInfo`

``` purescript
type AllFleetInfo a = Array (Maybe a)
```

#### `DamageAnalyzer`

``` purescript
type DamageAnalyzer = Battle -> AllFleetInfo DamageTookInfo -> AllFleetInfo DamageTookInfo
```

#### `noDamage`

``` purescript
noDamage :: Int -> DamageTookInfo
```

#### `battleStart`

``` purescript
battleStart :: Battle -> AllFleetInfo DamageTookInfo
```

#### `analyzeBattle`

``` purescript
analyzeBattle :: Battle -> AllFleetInfo DamageTookInfo
```

#### `analyzeNightBattle`

``` purescript
analyzeNightBattle :: NightBattle -> AllFleetInfo DamageTookInfoNight
```

#### `analyzeRawBattle`

``` purescript
analyzeRawBattle :: Foreign -> AllFleetInfo DamageTookInfo
```

#### `analyzeRawNightBattle`

``` purescript
analyzeRawNightBattle :: Foreign -> AllFleetInfo DamageTookInfoNight
```

#### `analyzeRawBattleJS`

``` purescript
analyzeRawBattleJS :: Foreign -> Array (Nullable DamageTookInfo)
```

#### `analyzeRawNightBattleJS`

``` purescript
analyzeRawNightBattleJS :: Foreign -> Array (Nullable DamageTookInfoNight)
```

#### `applyDamageVector`

``` purescript
applyDamageVector :: DamageVector -> AllFleetInfo DamageTookInfo -> AllFleetInfo DamageTookInfo
```

#### `analyzeBattleAlt`

``` purescript
analyzeBattleAlt :: Battle -> AllFleetInfo DamageTookInfo
```

#### `analyzeNightBattleAlt`

``` purescript
analyzeNightBattleAlt :: NightBattle -> AllFleetInfo DamageTookInfoNight
```


