## Module KanColle.Util

This module stores utility functions

#### `jsonStringify`

``` purescript
jsonStringify :: forall a. a -> String
```

`JSON.stringify`

#### `throwWith`

``` purescript
throwWith :: forall a b. a -> b
```

throw anything, raise exceptions from pure code

#### `todo`

``` purescript
todo :: forall a b. b -> a
```

use it as a placeholder for any not yet implemented

#### `times`

``` purescript
times :: forall m. (Monoid m) => Int -> m -> m
```

replicate a `Monoid` for a given number of times and `mconcat` the result

#### `chooseN`

``` purescript
chooseN :: forall a f. (Foldable f) => f a -> Int -> Array (List a)
```

`chooseN xs n` non-deterministically chooses `n` values from `xs`.
It is recommended to use only small values on `n` (0,1,2,3).

#### `peekSTArrayUnsafe`

``` purescript
peekSTArrayUnsafe :: forall a h r. STArray h a -> Int -> Eff (st :: ST h | r) a
```

`peekSTArray` without array bound checks

#### `pokeSTArrayUnsafe`

``` purescript
pokeSTArrayUnsafe :: forall a h r. STArray h a -> Int -> a -> Eff (st :: ST h | r) Unit
```

`pokeSTArray` without array bound checks

#### `traceLog`

``` purescript
traceLog :: forall a b. a -> (Unit -> b) -> b
```

`console.log` anything

#### `traceWarn`

``` purescript
traceWarn :: forall a b. a -> (Unit -> b) -> b
```

`console.warn` anything

#### `LR`

``` purescript
type LR a = { left :: a, right :: a }
```

something that has both "left" part and "right" part

#### `fleetSplit`

``` purescript
fleetSplit :: forall a. Boolean -> Array a -> LR (Array a)
```

`fleetSplit cutHead xs` cuts `xs` into two 6-element arrays.
`xs` has to be of length 12 (or 13 when `cutHead` is true)
if `cutHead` is true, the first value of `xs` is dropped before cutting.

#### `lrMap`

``` purescript
lrMap :: forall a b. (a -> b) -> LR a -> LR b
```

apply a function to both parts of an `LR`

#### `memptyLR`

``` purescript
memptyLR :: forall m. (Monoid m) => LR m
```

`mempty` for `LR`

#### `lrAppend`

``` purescript
lrAppend :: forall m. (Monoid m) => LR m -> LR m -> LR m
```

`append` for `LR`

#### `lrOnlyLeft`

``` purescript
lrOnlyLeft :: forall m. (Monoid m) => m -> LR m
```

lift some Monoid that has only left part into `LR`

#### `lrOnlyRight`

``` purescript
lrOnlyRight :: forall m. (Monoid m) => m -> LR m
```

lift some Monoid that has only right part into `LR`


