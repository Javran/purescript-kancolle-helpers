## Module KanColle.KCAPI.Master

#### `Master`

``` purescript
type Master = { api_mst_ship :: Array MstShip, api_mst_shipupgrade :: Array MstShipUpgrade }
```

#### `MstShip`

``` purescript
type MstShip = { api_name :: String, api_id :: Int, api_afterlv :: Int, api_afterfuel :: Int, api_afterbull :: Int, api_aftershipid :: String }
```

#### `MstShipUpgrade`

``` purescript
type MstShipUpgrade = { api_id :: Int, api_current_ship_id :: Int, api_catapult_count :: Int, api_drawing_count :: Int }
```


