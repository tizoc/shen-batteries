# Dictionaries

Public API and types for internal Shen dictionary data structure.

TODO

## API

### Construction

#### dict.make

**Type**: `number --> (dict.t Key Value)`

`(dict.make N)`

### Predicates

#### dict.is?

**Type**: `A --> boolean`

`(dict.is? X)`

### Access

#### dict.get

**Type**: `(dict.t Key Value) --> Key --> Value`

`(dict.get Dict Key)`

### Modification

#### dict.set

**Type**: `(dict.t Key Value) --> Key --> Value --> Value`

`(dict.set Dict Key Value)`

### Traversal

#### dict.fold

**Type**: `(Key --> Value --> A --> A) --> (dict.t Key Value) --> A --> A`

`(dict.set Dict Key Value)`

#### dict.keys

**Type**: `(dict.t Key Value) --> (list Key)`

`(dict.keys Dict)`

#### dict.values

**Type**: `(dict.t Key Value) --> (list Value)`

`(dict.values Dict)`

