# Dictionaries

Public API and types for internal Shen dictionary data structure.

## API

### Construction

#### `dict.make` : `number --> (dict.t Key Value)`

`(dict.make SizeHint)` creates a new dictionary. `SizeHint` is a size hint for the underlying implementation, the recommended value is equal to the expected amount values this dict will hold.

### Predicates

#### `dict.is?` : `A --> boolean`

`(dict.is? X)` returns `true` if `X` is a dictionary, and `false` otherwise.

### Access

#### `dict.get` : `(dict.t Key Value) --> Key --> Value`

`(dict.get Dict Key)` returns the value in `Dict` associated with `Key`. Raises an error if no value associated with `Key` is found.

### Modification

#### `dict.set` : `(dict.t Key Value) --> Key --> Value --> Value`

`(dict.set Dict Key Value)` associates `Value` with `Key` in `Dict`.

#### `dict.delete` : `(dict.t Key Value) --> Key --> Key`

`(dict.delete Dict Key)` deletes the value associated with `Key` in `Dict`.

### Traversal

#### `dict.fold` : `(Key --> Value --> A --> A) --> (dict.t Key Value) --> A --> A`

`(dict.fold F Dict Accum)` calls `(F Key Value Accum)` for each association in `Dict`. The return value of each call to `F` is used as `Accum` in the next call. Returns the last result of calling `F`.

#### `dict.keys` : `(dict.t Key Value) --> (list Key)`

`(dict.keys Dict)` returns a list of all keys in `Dict`.

#### `dict.values` : `(dict.t Key Value) --> (list Value)`

`(dict.values Dict)` returns a list of all values in `Dict`.

