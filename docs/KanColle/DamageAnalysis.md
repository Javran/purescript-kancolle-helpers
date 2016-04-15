## Module KanColle.DamageAnalysis


### Re-exported from KanColle.DamageAnalysis.Damage:

#### `Damage`

``` purescript
newtype Damage
```

a `Damage` represents a damage value to be applied
to a `Ship`

##### Instances
``` purescript
Semigroup Damage
Monoid Damage
```

#### `addDamage`

``` purescript
addDamage :: Damage -> Int -> Damage
```

add damage value to an existing `Damage`

#### `applyDamage`

``` purescript
applyDamage :: Damage -> Ship -> Ship
```

apply `Damage` to a `Ship`, DameCon will be used if applicable.

#### `damageToInt`

``` purescript
damageToInt :: Damage -> Int
```

converts `Damage` into the damage value

#### `mergeDamage`

``` purescript
mergeDamage :: Damage -> Damage -> Damage
```

create new `Damage` by applying two `Damage`s in order

#### `mkDamage`

``` purescript
mkDamage :: Int -> Damage
```

create `Damage` from damage value

### Re-exported from KanColle.DamageAnalysis.DamageAnalysis:

#### `analyzeBattle`

``` purescript
analyzeBattle :: Array (Maybe DameCon) -> Battle -> NormalFleetInfo ShipResult
```

#### `analyzeCTFBattle`

``` purescript
analyzeCTFBattle :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfo ShipResult
```

#### `analyzeCombinedNightBattle`

``` purescript
analyzeCombinedNightBattle :: Array (Maybe DameCon) -> Battle -> NormalFleetInfo ShipResult
```

#### `analyzeNightBattle`

``` purescript
analyzeNightBattle :: Array (Maybe DameCon) -> Battle -> NormalFleetInfo ShipResult
```

#### `analyzeSTFBattle`

``` purescript
analyzeSTFBattle :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfo ShipResult
```

#### `analyzeTECFBattle`

``` purescript
analyzeTECFBattle :: Array (Maybe DameCon) -> Battle -> CombinedFleetInfo ShipResult
```

### Re-exported from KanColle.DamageAnalysis.DamageVector:

#### `CombinedDamageVector`

``` purescript
type CombinedDamageVector = CombinedBattle DamageVector
```

`DamageVector` for battles involving comined fleets

#### `DamageVector`

``` purescript
newtype DamageVector
```

`DamageVector` is a 6-element array of `Damage`s

##### Instances
``` purescript
Semigroup DamageVector
Monoid DamageVector
```

#### `FleetRole`

``` purescript
data FleetRole
  = FRMain
  | FREscort
  | FRSupport
```

ally fleet's role in this battle

#### `NormalDamageVector`

``` purescript
type NormalDamageVector = NormalBattle DamageVector
```

`DamageVector` for normal battles

#### `applyCombinedDamageVector`

``` purescript
applyCombinedDamageVector :: CombinedDamageVector -> CombinedFleetInfo Ship -> CombinedFleetInfo Ship
```

apply `CombinedDamageVector` on a combined fleet of ships (including enemy ships)

#### `applyDamageVector`

``` purescript
applyDamageVector :: DamageVector -> FleetInfo Ship -> FleetInfo Ship
```

apply a single `DamageVector` on a single fleet

#### `applyNormalDamageVector`

``` purescript
applyNormalDamageVector :: NormalDamageVector -> NormalFleetInfo Ship -> NormalFleetInfo Ship
```

apply `NormalDamageVector` on a normal fleet of ships (including enemy ships)

#### `calcHougekiDamage`

``` purescript
calcHougekiDamage :: Hougeki -> LR DamageVector
```

calculate damage from hougeki (shelling) stages

#### `calcKoukuDamage`

``` purescript
calcKoukuDamage :: Kouku -> LR DamageVector
```

calculate damage from kouku (aerial) stages

#### `calcKoukuDamageCombined`

``` purescript
calcKoukuDamageCombined :: Kouku -> DamageVector
```

calculate damage from kouku (aerial) stages (combined fleet)
note that only escort fleet is taking damage. so we just need DamageVector

#### `calcRaigekiDamage`

``` purescript
calcRaigekiDamage :: Raigeki -> LR DamageVector
```

calculate damage from raigeki (torpedo) stages

#### `calcSupportAirAttackDamage`

``` purescript
calcSupportAirAttackDamage :: SupportAirInfo -> DamageVector
```

calculate damage from support airstrike stages.
note that only enemy is taking damage so this results in
a single `DamageVector`.

#### `calcSupportHouraiDamage`

``` purescript
calcSupportHouraiDamage :: SupportHouraiInfo -> DamageVector
```

calculate damage from support shelling stages.
note that only enemy is taking damage so this results in
a single `DamageVector`.

#### `getDV`

``` purescript
getDV :: DamageVector -> Array Damage
```

fetches the array inside `DamageVector`

#### `mkDV`

``` purescript
mkDV :: Array Damage -> DamageVector
```

creates `DamageVector` from arrays.
array has to be of length 6

#### `toCombined`

``` purescript
toCombined :: FleetRole -> LR DamageVector -> CombinedDamageVector
```

`toCombined role dv` converts a `LR DamageVector` whose left part
is playing role `role` into `CombinedDamageVector`

### Re-exported from KanColle.DamageAnalysis.Stages:

#### `battleCarrierTaskForceDV`

``` purescript
battleCarrierTaskForceDV :: Battle -> CombinedDamageVector
```

get `CombinedDamageVector` of a carrier task force battle
note that transport escort battle uses this `CombinedDamageVector` as well

#### `battleDV`

``` purescript
battleDV :: Battle -> NormalDamageVector
```

get `NormalDamageVector` of a regular / aerial battle from battle data
a regular battle consists of the following stages:

* `kouku`  (aerial battle)
* `kouku2` (aerial battle)
* `supportAirInfo` (airstrike from support expedition)
* `supportHouraiInfo` (shelling attack from support expedition)
* `opening` (openning torpedo attack)
* `hougeki1` (first shelling stage)
* `hougeki2` (second shelling stage)
* `hougeki3` (third shelling stage, always empty for regular battles)
* `raigeki` (closing torpedo attack)

#### `battleSurfaceTaskForceDV`

``` purescript
battleSurfaceTaskForceDV :: Battle -> CombinedDamageVector
```

get `CombinedDamageVector` of a surface task force battle

#### `koukuCombinedDV`

``` purescript
koukuCombinedDV :: Battle -> DamageVector
```

#### `koukuDV`

``` purescript
koukuDV :: Battle -> LR DamageVector
```

get `DamageVector` of kouku stage from battle data
all the names in this module are kept consistent with functions in
`KanColle.DamageAnalysis.DamageVector`.

#### `nightBattleDV`

``` purescript
nightBattleDV :: Battle -> NormalDamageVector
```

get `NormalDamageVector` of a night battle
a night battle involves only `hougeki` (shelling stage)

#### `supportAirAttackDV`

``` purescript
supportAirAttackDV :: Battle -> DamageVector
```

#### `supportHouraiDV`

``` purescript
supportHouraiDV :: Battle -> DamageVector
```

### Re-exported from KanColle.DamageAnalysis.Types:

#### `CombinedBattle`

``` purescript
type CombinedBattle a = { main :: a, escort :: a, enemy :: a }
```

#### `CombinedFleetInfo`

``` purescript
type CombinedFleetInfo a = CombinedBattle (FleetInfo a)
```

#### `DameCon`

``` purescript
data DameCon
  = RepairTeam
  | RepairGoddess
```

##### Instances
``` purescript
Eq DameCon
Show DameCon
```

#### `FleetInfo`

``` purescript
type FleetInfo a = Array (Maybe a)
```

#### `NormalBattle`

``` purescript
type NormalBattle a = { main :: a, enemy :: a }
```

#### `NormalFleetInfo`

``` purescript
type NormalFleetInfo a = NormalBattle (FleetInfo a)
```

#### `Ship`

``` purescript
type Ship = { hp :: Int, fullHp :: Int, sunk :: Boolean, dameCon :: Maybe DameCon }
```

#### `ShipResult`

``` purescript
type ShipResult = { hp :: Int, sunk :: Boolean, dameConConsumed :: Boolean }
```

#### `appCombinedBattle`

``` purescript
appCombinedBattle :: forall a b. CombinedBattle (a -> b) -> CombinedBattle a -> CombinedBattle b
```

#### `appNormalBattle`

``` purescript
appNormalBattle :: forall a b. NormalBattle (a -> b) -> NormalBattle a -> NormalBattle b
```

#### `dupAsCombinedBattle`

``` purescript
dupAsCombinedBattle :: forall a. a -> CombinedBattle a
```

#### `dupAsNormalBattle`

``` purescript
dupAsNormalBattle :: forall a. a -> NormalBattle a
```

#### `getShipResult`

``` purescript
getShipResult :: Ship -> Ship -> ShipResult
```

#### `lrToNormal`

``` purescript
lrToNormal :: forall a. LR a -> NormalBattle a
```

#### `mapCombinedBattle`

``` purescript
mapCombinedBattle :: forall a b. (a -> b) -> CombinedBattle a -> CombinedBattle b
```

#### `mapNormalBattle`

``` purescript
mapNormalBattle :: forall a b. (a -> b) -> NormalBattle a -> NormalBattle b
```

#### `mkShip`

``` purescript
mkShip :: Int -> Int -> Maybe DameCon -> Ship
```

