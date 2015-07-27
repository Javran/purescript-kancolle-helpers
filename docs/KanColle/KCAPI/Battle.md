## Module KanColle.KCAPI.Battle

#### `Battle`

``` purescript
type Battle = { api_nowhps :: Array Int, api_stage_flag :: Array Int, api_stage_flag2 :: Array Int, api_kouku :: Kouku, api_kouku2 :: Kouku, api_opening_flag :: Int, api_opening_atack :: Raigeki, api_hourai_flag :: Array Int, api_hougeki1 :: Hougeki, api_hougeki2 :: Hougeki, api_hougeki3 :: Hougeki, api_raigeki :: Raigeki, api_hougeki :: Hougeki }
```

#### `NightBattle`

``` purescript
type NightBattle = Battle
```

#### `Kouku`

``` purescript
type Kouku = { api_stage3 :: KoukuStage3 }
```

#### `KoukuStage3`

``` purescript
type KoukuStage3 = { api_fdam :: Array Number, api_edam :: Array Number }
```

#### `Hougeki`

``` purescript
type Hougeki = { api_df_list :: Array Foreign, api_damage :: Array Foreign }
```

#### `Raigeki`

``` purescript
type Raigeki = { api_fdam :: Array Number, api_edam :: Array Number }
```

#### `hasField`

``` purescript
hasField :: String -> Battle -> Boolean
```

#### `hasKouku`

``` purescript
hasKouku :: Battle -> Boolean
```

#### `hasKouku2`

``` purescript
hasKouku2 :: Battle -> Boolean
```

#### `hasHourai`

``` purescript
hasHourai :: Battle -> Boolean
```

#### `hasHougeki`

``` purescript
hasHougeki :: Battle -> Boolean
```

#### `getKouku`

``` purescript
getKouku :: Battle -> Maybe Kouku
```

#### `getKouku2`

``` purescript
getKouku2 :: Battle -> Maybe Kouku
```

#### `getOpeningAttack`

``` purescript
getOpeningAttack :: Battle -> Maybe Raigeki
```

#### `getHougeki1`

``` purescript
getHougeki1 :: Battle -> Maybe Hougeki
```

#### `getHougeki2`

``` purescript
getHougeki2 :: Battle -> Maybe Hougeki
```

#### `getHougeki3`

``` purescript
getHougeki3 :: Battle -> Maybe Hougeki
```

#### `getRaigeki`

``` purescript
getRaigeki :: Battle -> Maybe Raigeki
```

#### `getHougeki`

``` purescript
getHougeki :: Battle -> Maybe Hougeki
```


