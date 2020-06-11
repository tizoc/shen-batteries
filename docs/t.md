# Core type rules

A few basic type rules that are useful for most programs.

## Types

### `t.unit` 

`unit` (alias of `unit`) is a type inhabited by every value. It is useful when writting functions where the type of the inputs is not known, or when the result is discarded. Use carefully.

### `t.verified-objects`

Adds `verified` rules for the `number?`, `string?`, `symbol?` and `boolean?` predicate, and also for `and` expressions (`(and X Y)` adds `verified` for the `X` and `Y` expressions if there is a rule defined for them).

### `t.verified-if`

This rule extends the typechecker so that when the `True` branch of `(if Test True False)` expressions is typechecked, any `verified` rules that result from `Test` are added to the

Example `(if (number? X) (+ X 2) 0)` for an `X` of unkown type would not typecheck without this rule, but `X` would.

### `t.or`

A value inhabits the type `(or t1 t2)` if it inhabits `t1` or `t2`.
