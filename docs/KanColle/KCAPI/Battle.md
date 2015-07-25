## Module KanColle.KCAPI.Battle

#### `Battle`

``` purescript
type Battle = { api_nowhps :: Array Int, api_stage_flag :: Array Int, api_kouku :: Kouku, api_opening_flag :: Int, api_opening_atack :: Raigeki, api_hourai_flag :: Array Int, api_hougeki1 :: Hougeki, api_hougeki2 :: Hougeki, api_hougeki3 :: Hougeki, api_raigeki :: Raigeki }
```

#### `NightBattle`

``` purescript
type NightBattle = { api_nowhps :: Array Int, api_hougeki :: Hougeki }
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


