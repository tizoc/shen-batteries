# Boxes

Boxes are mutable references to a value.

## API

### Construction

#### `box.make`

**Type**: `A --> (box.t A)`

`(box.make Value)` returns a new box containing `Value`.

### Predicates

#### `box.box?`

**Type**: `A --> boolean`

`(box.box? X)` returns `true` if X is a box, `false` otherwise.

### Access

#### `box.unbox`

**Type**: `(box.t A) --> A`

`(box.unbox Box)` returns the value inside `Box`.

### Modification

#### `box.put`

**Type**: `(box.t A) --> A --> (box.t A)`

`(box.put Box Value)` stores `Value` inside `Box`.

#### `box.modify`

**Type**: `(A --> A) --> (box.t A) --> (box.t A)`

`(box.modify F Box)` stores the result of `(box.put (F (box.unbox Box)))` inside `Box`.

