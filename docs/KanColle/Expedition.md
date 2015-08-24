## Module KanColle.Expedition

#### `ShipRequirement`

``` purescript
data ShipRequirement
  = ShipLevel Int
  | ShipTypeOneOf (Array SType)
```

#### `FleetRequirement`

``` purescript
data FleetRequirement
  = Flagship ShipRequirement
  | FleetLevel Int
  | FleetDrum Int
  | FleetShipWithDrum Int
  | FleetSTypeCount Int (Array SType)
  | FleetShipCount Int
```

#### `explainShipRequirement`

``` purescript
explainShipRequirement :: ShipRequirement -> String
```

#### `explainFleetRequirement`

``` purescript
explainFleetRequirement :: FleetRequirement -> String
```

#### `Ship`

``` purescript
type Ship a = { ammo :: Int, morale :: Int, stype :: SType, level :: Int, drumCount :: Int | a }
```

#### `RawShip`

``` purescript
type RawShip a = { ammo :: Int, morale :: Int, stype :: String, level :: Int, drumCount :: Int | a }
```

#### `Fleet`

``` purescript
type Fleet a = Array (Ship a)
```

#### `fromRawShip`

``` purescript
fromRawShip :: forall a. RawShip a -> Ship a
```

#### `fromRawFleet`

``` purescript
fromRawFleet :: forall a. Array (RawShip a) -> Fleet a
```

#### `unsatisfiedRequirements`

``` purescript
unsatisfiedRequirements :: forall a. Int -> Fleet a -> Array FleetRequirement
```

#### `explainRequirements`

``` purescript
explainRequirements :: Array FleetRequirement -> Array String
```

#### `checkExpedition`

``` purescript
checkExpedition :: forall a. Int -> Fleet a -> Boolean
```

#### `validExpeditionId`

``` purescript
validExpeditionId :: Int -> Boolean
```

#### `getAvailableExpeditions`

``` purescript
getAvailableExpeditions :: forall a. Fleet a -> Array Int
```


