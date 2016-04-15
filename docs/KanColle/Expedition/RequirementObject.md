## Module KanColle.Expedition.RequirementObject

#### `STypeReq`

``` purescript
type STypeReq = { stypeReqCount :: Int, stypeOneOf :: Array SType }
```

#### `STypeReqObj`

``` purescript
type STypeReqObj = { stypeReqCount :: Int, stypeOneOf :: Array String }
```

#### `RequirementPack`

``` purescript
type RequirementPack = { flagShipLevel :: Int, shipCount :: Int, flagShipTypeOf :: Maybe (Array SType), levelCount :: Maybe Int, drumCount :: Maybe Int, drumCarrierCount :: Maybe Int, fleetSType :: Array STypeReq }
```

#### `ResultPack`

``` purescript
type ResultPack (f :: * -> *) = { flagShipLevel :: Boolean, shipCount :: Boolean, flagShipTypeOf :: f Boolean, levelCount :: f Boolean, drumCount :: f Boolean, drumCarrierCount :: f Boolean, fleetSType :: Array Boolean }
```

#### `RequirementObject`

``` purescript
type RequirementObject = { flagShipLevel :: Int, shipCount :: Int, flagShipTypeOf :: Nullable (Array String), levelCount :: Nullable Int, drumCount :: Nullable Int, drumCarrierCount :: Nullable Int, fleetSType :: Array STypeReqObj }
```

#### `ResultObject`

``` purescript
type ResultObject = ResultPack Nullable
```

#### `dummyRequirementPack`

``` purescript
dummyRequirementPack :: RequirementPack
```

#### `fromExpeditionRequirement`

``` purescript
fromExpeditionRequirement :: ExpeditionRequirement -> RequirementPack
```

#### `requirementPackToObj`

``` purescript
requirementPackToObj :: RequirementPack -> RequirementObject
```

#### `allRequirements`

``` purescript
allRequirements :: Array { id :: Int, val :: RequirementObject }
```

#### `getExpeditionRequirementPack`

``` purescript
getExpeditionRequirementPack :: Int -> RequirementPack
```

#### `getExpeditionRequirementObject`

``` purescript
getExpeditionRequirementObject :: Int -> RequirementObject
```

#### `checkWithRequirementPack`

``` purescript
checkWithRequirementPack :: forall a. RequirementPack -> Fleet a -> ResultPack Maybe
```

#### `resultPackToObject`

``` purescript
resultPackToObject :: ResultPack Maybe -> ResultPack Nullable
```


