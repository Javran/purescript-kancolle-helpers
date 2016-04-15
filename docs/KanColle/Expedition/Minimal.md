## Module KanColle.Expedition.Minimal

#### `ShipMaxCost`

``` purescript
type ShipMaxCost = { fuel :: Int, ammo :: Int }
```

#### `ECost`

``` purescript
newtype ECost
  = ECost { shipCost :: Array ShipMaxCost, fleet :: Fleet (note :: Maybe String) }
```

minimal cost of an expedition, just for the purpose of estimation
despite that we are trying to provide a possible ship composition
that minimizes the cost, the suggestion might not actually be possible
due to many factors (e.g. other running expeditions are using the same ship,
or you have not yet obtained the ship)

##### Instances
``` purescript
Semigroup ECost
Monoid ECost
```

#### `getECost`

``` purescript
getECost :: ECost -> { shipCost :: Array ShipMaxCost, fleet :: Fleet (note :: Maybe String) }
```

#### `dummyShip`

``` purescript
dummyShip :: SType -> Ship (note :: Maybe String)
```

#### `shipWithNote`

``` purescript
shipWithNote :: SType -> String -> Ship (note :: Maybe String)
```

#### `mkECost`

``` purescript
mkECost :: SType -> Int -> Int -> ECost
```

#### `mkECostWithNote`

``` purescript
mkECostWithNote :: SType -> Int -> Int -> String -> ECost
```

#### `ddCost`

``` purescript
ddCost :: Int -> ECost
```

minimal DD cost, achivable by taking any of Mutsuki class ships

#### `ssCost`

``` purescript
ssCost :: Int -> ECost
```

minimal SS/SSV cost, achivable by taking
Maruyu and I-168 (Kai) / I-58 / I-19 / I-8 / U-511 / Ro-500

#### `cvlCost`

``` purescript
cvlCost :: Int -> ECost
```

minimal CVL cost, achivable by taking
Houshou / Shouhou class (without Kai).
Input is expected to be within [1..3]
as the minimal expedition
never requires more than 3 CVLs at a time

#### `avCost`

``` purescript
avCost :: Int -> ECost
```

minimal AV cost, input takes value [1..4]

#### `bbvCost`

``` purescript
bbvCost :: Int -> ECost
```

minimal BBV cost, input takes value [1..4]
achivable by taking any non-Kai-Ni BBV

#### `caCost`

``` purescript
caCost :: Int -> ECost
```

minimal CA cost, achivable by taking
Furutaka class + Aoba class, takes [1..4]

#### `clCost`

``` purescript
clCost :: Int -> ECost
```

minimal CL cost, achivable by taking
Tenryuu class + non-Kai Kuma class

#### `taigei`

``` purescript
taigei :: ECost
```

the sole submarine tender (AS) is Taigei

#### `katori`

``` purescript
katori :: ECost
```

the sole training cruiser (CT) is Katori

#### `fillSS`

``` purescript
fillSS :: Int -> ECost -> ECost
```

fill in submarines to meet a ship number requirement

#### `getExpeditionMinCost`

``` purescript
getExpeditionMinCost :: Int -> ECost
```

#### `pprFleetNotes`

``` purescript
pprFleetNotes :: Fleet (note :: Maybe String) -> String
```

#### `minimalCostMarkdownTable`

``` purescript
minimalCostMarkdownTable :: String
```

generate markdown string for pretty printing the minimal cost table


