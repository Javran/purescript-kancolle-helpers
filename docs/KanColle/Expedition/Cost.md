## Module KanColle.Expedition.Cost

#### `Cost`

``` purescript
type Cost = { fuel :: Number, ammo :: Number, time :: Int }
```

Expedition cost. `fuel` and `ammo` are floating numbers
(valid values are taken from `0.0` to `1.0`) representing
percentage of fuel / ammo running an expedition would cost.
And `time` records the total time (in minutes) of running this expedition.

#### `calcCost`

``` purescript
calcCost :: Cost -> { fuel :: Int, ammo :: Int } -> { fuel :: Int, ammo :: Int }
```

#### `getExpeditionCost`

``` purescript
getExpeditionCost :: Int -> Cost
```

input a valid expedition id and get expedition cost.


