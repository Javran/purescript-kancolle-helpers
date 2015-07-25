## Module KanColle.DamageAnalysis

#### `DamageTookInfo`

``` purescript
type DamageTookInfo = { aerial :: Int, opening :: Int, hougeki1 :: Int, hougeki2 :: Int, hougeki3 :: Int, closing :: Int, currentHp :: Int }
```

#### `DamageTookInfoNight`

``` purescript
type DamageTookInfoNight = { hougeki :: Int, currentHp :: Int }
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

#### `battleStartNight`

``` purescript
battleStartNight :: NightBattle -> AllFleetInfo DamageTookInfoNight
```

#### `damageNormalize`

``` purescript
damageNormalize :: Array Number -> Array Int
```

#### `calcAerial`

``` purescript
calcAerial :: DamageAnalyzer
```

#### `calcHougeki`

``` purescript
calcHougeki :: Hougeki -> Int -> AllFleetInfo DamageTookInfo -> AllFleetInfo DamageTookInfo
```

#### `calcNightHougeki`

``` purescript
calcNightHougeki :: NightBattle -> AllFleetInfo DamageTookInfoNight -> AllFleetInfo DamageTookInfoNight
```

#### `calcRaigeki`

``` purescript
calcRaigeki :: Raigeki -> Int -> AllFleetInfo DamageTookInfo -> AllFleetInfo DamageTookInfo
```

#### `calcOpeningRaigeki`

``` purescript
calcOpeningRaigeki :: DamageAnalyzer
```

#### `calcClosingRaigeki`

``` purescript
calcClosingRaigeki :: DamageAnalyzer
```

#### `calcAllHougeki`

``` purescript
calcAllHougeki :: DamageAnalyzer
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


