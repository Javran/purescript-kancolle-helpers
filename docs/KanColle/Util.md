## Module KanColle.Util

#### `times1p`

``` purescript
times1p :: forall m. (Semigroup m) => Int -> m -> m
```

#### `times`

``` purescript
times :: forall m. (Monoid m) => Int -> m -> m
```

#### `peekSTArrayUnsafe`

``` purescript
peekSTArrayUnsafe :: forall a h r. STArray h a -> Int -> Eff (st :: ST h | r) a
```

#### `pokeSTArrayUnsafe`

``` purescript
pokeSTArrayUnsafe :: forall a h r. STArray h a -> Int -> a -> Eff (st :: ST h | r) Unit
```

#### `heapify`

``` purescript
heapify :: forall a h r. (a -> a -> Ordering) -> Array a -> Eff (st :: ST h | r) (STArray h a)
```

#### `startHeapifyDown`

``` purescript
startHeapifyDown :: forall a h r. (a -> a -> Ordering) -> STArray h a -> Int -> Int -> Eff (st :: ST h | r) Unit
```

#### `heapifyDown`

``` purescript
heapifyDown :: forall a h r. (a -> a -> Ordering) -> STArray h a -> Int -> Int -> a -> Eff (st :: ST h | r) Unit
```

#### `deleteMax`

``` purescript
deleteMax :: forall a h r. (a -> a -> Ordering) -> STArray h a -> Int -> Eff (st :: ST h | r) a
```

#### `heapSort`

``` purescript
heapSort :: forall a. (a -> a -> Ordering) -> Array a -> Array a
```

#### `heapSortSafe`

``` purescript
heapSortSafe :: forall a. (a -> a -> Ordering) -> Array a -> Array a
```

#### `sortByThenTake`

``` purescript
sortByThenTake :: forall a. (a -> a -> Ordering) -> Int -> Array a -> Array a
```

#### `sortByThenTakeQuick`

``` purescript
sortByThenTakeQuick :: forall a. (a -> a -> Ordering) -> Int -> Array a -> Array a
```


