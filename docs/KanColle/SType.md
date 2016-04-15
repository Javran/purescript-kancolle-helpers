## Module KanColle.SType

#### `eqSType`

``` purescript
eqSType :: SType -> SType -> Boolean
```


### Re-exported from KanColle.Generated.SType:

#### `SType`

``` purescript
data SType
  = DDE
  | DD
  | CL
  | CLT
  | CA
  | CAV
  | CVL
  | FBB
  | BB
  | BBV
  | CV
  | XBB
  | SS
  | SSV
  | AP
  | AV
  | LHA
  | CVB
  | AR
  | AS
  | CT
  | AO
  | Unknown String
```

#### `fromInt`

``` purescript
fromInt :: Int -> SType
```

#### `readSType`

``` purescript
readSType :: String -> SType
```

#### `showSType`

``` purescript
showSType :: SType -> String
```

#### `toInt`

``` purescript
toInt :: SType -> Int
```

