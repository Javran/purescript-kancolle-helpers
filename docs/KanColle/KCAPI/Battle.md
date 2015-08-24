## Module KanColle.KCAPI.Battle

#### `Battle`

``` purescript
type Battle = { api_nowhps :: Array Int, api_nowhps_combined :: Array Int, api_stage_flag :: Array Int, api_stage_flag2 :: Array Int, api_kouku :: Kouku, api_kouku2 :: Kouku, api_opening_flag :: Int, api_opening_atack :: Raigeki, api_hourai_flag :: Array Int, api_hougeki1 :: Hougeki, api_hougeki2 :: Hougeki, api_hougeki3 :: Hougeki, api_raigeki :: Raigeki, api_hougeki :: Hougeki, api_support_flag :: Int, api_support_info :: SupportInfo }
```

#### `Kouku`

``` purescript
type Kouku = { api_stage3 :: KoukuStage3, api_stage3_combined :: KoukuStage3 }
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

#### `SupportAirInfo`

``` purescript
type SupportAirInfo = { api_stage3 :: { api_edam :: Array Number } }
```

#### `SupportHouraiInfo`

``` purescript
type SupportHouraiInfo = { api_damage :: Array Number }
```

#### `SupportInfo`

``` purescript
type SupportInfo = { api_support_airatack :: SupportAirInfo, api_support_hourai :: SupportHouraiInfo }
```

#### `getInitHps`

``` purescript
getInitHps :: Battle -> Array (Maybe Int)
```

#### `getInitHpsCombined`

``` purescript
getInitHpsCombined :: Battle -> Array (Maybe Int)
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

#### `getSupportFlag`

``` purescript
getSupportFlag :: Battle -> Maybe Int
```

#### `getSupportAirInfo`

``` purescript
getSupportAirInfo :: Battle -> Maybe SupportAirInfo
```

#### `getSupportHouraiInfo`

``` purescript
getSupportHouraiInfo :: Battle -> Maybe SupportHouraiInfo
```

#### `getKouku2`

``` purescript
getKouku2 :: Battle -> Maybe Kouku
```

#### `getHouraiFlags`

``` purescript
getHouraiFlags :: Battle -> Maybe (Array Int)
```

#### `checkHouraiFlag`

``` purescript
checkHouraiFlag :: Int -> Battle -> Maybe Unit
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

#### `getHougeki1CT`

``` purescript
getHougeki1CT :: Battle -> Maybe Hougeki
```

#### `getRaigekiCT`

``` purescript
getRaigekiCT :: Battle -> Maybe Raigeki
```

#### `getHougeki2CT`

``` purescript
getHougeki2CT :: Battle -> Maybe Hougeki
```

#### `getHougeki3CT`

``` purescript
getHougeki3CT :: Battle -> Maybe Hougeki
```


