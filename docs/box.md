# Boxes

Boxes are mutable references to a value.

## API

### Construction

#### `box.make` : `A --> (box.t A)`

`(box.make Value)` returns a new box containing `Value`.

### Predicates

#### `box.box?` : `A --> boolean`

`(box.box? X)` returns `true` if X is a box, `false` otherwise.

### Access

#### `box.unbox` : `(box.t A) --> A`

`(box.unbox Box)` returns the value inside `Box`.

### Modification

#### `box.put` : `(box.t A) --> A --> (box.t A)`

`(box.put Box Value)` stores `Value` inside `Box`.

#### `box.modify` : `(A --> A) --> (box.t A) --> (box.t A)`

`(box.modify F Box)` stores the result of `(box.put (F (box.unbox Box)))` inside `Box`.

