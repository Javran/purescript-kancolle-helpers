## Module KanColle.DamageAnalysis

#### `DamageTookInfo`

``` purescript
type DamageTookInfo = { currentHp :: Int }
```

#### `DamageTookInfoNight`

``` purescript
type DamageTookInfoNight = DamageTookInfo
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

#### `CombinedFleetInfo`

``` purescript
type CombinedFleetInfo a = { main :: Array (Maybe a), escort :: Array (Maybe a), enemy :: Array (Maybe a) }
```

#### `battleStart`

``` purescript
battleStart :: Battle -> AllFleetInfo DamageTookInfo
```

#### `battleCombinedStart`

``` purescript
battleCombinedStart :: Battle -> CombinedFleetInfo DamageTookInfo
```

#### `analyzeBattle`

``` purescript
analyzeBattle :: Battle -> AllFleetInfo DamageTookInfo
```

#### `analyzeNightBattle`

``` purescript
analyzeNightBattle :: Battle -> AllFleetInfo DamageTookInfoNight
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

#### `applyCombinedDamageVector`

``` purescript
applyCombinedDamageVector :: CombinedDamageVector -> CombinedFleetInfo DamageTookInfo -> CombinedFleetInfo DamageTookInfo
```

#### `analyzeSurfaceTaskForceBattle`

``` purescript
analyzeSurfaceTaskForceBattle :: Battle -> CombinedFleetInfo DamageTookInfo
```

#### `analyzeRawSurfaceTaskForceBattle`

``` purescript
analyzeRawSurfaceTaskForceBattle :: Foreign -> CombinedFleetInfo DamageTookInfo
```

#### `analyzeRawSurfaceTaskForceBattleJS`

``` purescript
analyzeRawSurfaceTaskForceBattleJS :: Foreign -> { main :: Array (Nullable DamageTookInfo), escort :: Array (Nullable DamageTookInfo), enemy :: Array (Nullable DamageTookInfo) }
```

#### `analyzeCarrierTaskForceBattle`

``` purescript
analyzeCarrierTaskForceBattle :: Battle -> CombinedFleetInfo DamageTookInfo
```

#### `analyzeRawCarrierTaskForceBattle`

``` purescript
analyzeRawCarrierTaskForceBattle :: Foreign -> CombinedFleetInfo DamageTookInfo
```

#### `analyzeRawCarrierTaskForceBattleJS`

``` purescript
analyzeRawCarrierTaskForceBattleJS :: Foreign -> { main :: Array (Nullable DamageTookInfo), escort :: Array (Nullable DamageTookInfo), enemy :: Array (Nullable DamageTookInfo) }
```

#### `analyzeNightBattleCombined`

``` purescript
analyzeNightBattleCombined :: Battle -> AllFleetInfo DamageTookInfoNight
```

#### `analyzeRawNightBattleCombined`

``` purescript
analyzeRawNightBattleCombined :: Foreign -> AllFleetInfo DamageTookInfoNight
```

#### `analyzeRawNightBattleCombinedJS`

``` purescript
analyzeRawNightBattleCombinedJS :: Foreign -> Array (Nullable DamageTookInfoNight)
```


