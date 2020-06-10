# Dictionaries

Public API and types for internal Shen dictionary data structure.

## API

### Construction

#### `dict.make`

**Type**: `number --> (dict.t Key Value)`

`(dict.make SizeHint)` creates a new dictionary. `SizeHint` is a size hint for the underlying implementation, the recommended value is equal to the expected amount values this dict will hold.

### Predicates

#### `dict.is?`

**Type**: `A --> boolean`

`(dict.is? X)` returns `true` if `X` is a dictionary, and `false` otherwise.

### Access

#### `dict.get`

**Type**: `(dict.t Key Value) --> Key --> Value`

`(dict.get Dict Key)` returns the value in `Dict` associated with `Key`. Raises an error if no value associated with `Key` is found.

### Modification

#### `dict.set`

**Type**: `(dict.t Key Value) --> Key --> Value --> Value`

`(dict.set Dict Key Value)` associates `Value` with `Key` in `Dict`.

#### `dict.delete`

**Type**: `(dict.t Key Value) --> Key --> Key`

`(dict.delete Dict Key)` deletes the value associated with `Key` in `Dict`.

### Traversal

#### `dict.fold`

**Type**: `(Key --> Value --> A --> A) --> (dict.t Key Value) --> A --> A`

`(dict.fold F Dict Accum)` calls `(F Key Value Accum)` for each association in `Dict`. The return value of each call to `F` is used as `Accum` in the next call. Returns the last result of calling `F`.

#### `dict.keys`

**Type**: `(dict.t Key Value) --> (list Key)`

`(dict.keys Dict)` returns a list of all keys in `Dict`.

#### `dict.values`

**Type**: `(dict.t Key Value) --> (list Value)`

`(dict.values Dict)` returns a list of all values in `Dict`.

