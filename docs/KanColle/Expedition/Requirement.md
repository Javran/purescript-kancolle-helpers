## Module KanColle.Expedition.Requirement

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

#### `ExpeditionRequirement`

``` purescript
type ExpeditionRequirement = Array FleetRequirement
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

#### `checkShip`

``` purescript
checkShip :: forall a. ShipRequirement -> Ship a -> Boolean
```

#### `checkFleet`

``` purescript
checkFleet :: forall a. FleetRequirement -> Fleet a -> Boolean
```

#### `getExpeditionRequirement`

``` purescript
getExpeditionRequirement :: Int -> ExpeditionRequirement
```

#### `unsatisfiedRequirements`

``` purescript
unsatisfiedRequirements :: forall a. Int -> Fleet a -> Array FleetRequirement
```

#### `explainRequirements`

``` purescript
explainRequirements :: Array FleetRequirement -> Array String
```


