# Libraries loader

Libraries for Shen.

## Overview

TODO

## Declaring libraries

Libraries are declared with the `library.declare` special form, with syntax:

`(library.declare <LIBRARY-NAME> <LIBRARY-DIRECTIVE> ...)`

`<LIBRARY-NAME>` is a symbol, it must be unique, and will be used to reference this library.

Each `<LIBRARY-DIRECTIVE>` is one of:

- `(loads <TC-OR-FILE> ...)` declares which files to load when requiring this library. `<TC-OR-FILE>` is a string with the name of a file to load, `tc+` to enable type-checking before loading the files that come next, or `tc-` to disable type-checking before loading the files that come next. By default, `tc-` is assumed.
- `(requires <LIBRARY-NAME> ...)` declares the dependencies of this library. Each `<LIBRARY-NAME>` is the name of a library that will be required and activated before loading the files of the library being defined.
- `(provides-pattern-handlers <FUNCTION-NAME> ...)` declares any function defined in this library that will be activated to extend the pattern handler when this library is activated.

## API

### Loading libraries

`(library.use (cons <LIBRARY-NAME> (cons ... ())))` loads and activates every library named in the list. Performing `library.use` on a library that is already active has no effect.

`(library.require (cons <LIBRARY-NAME> (cons ... ())))` loads every library named in the list without performing any activation. Performing `library.require` on a library that has been required before has no effect.

Most of the time `library.use` will be used in user code, with `library.required` reserved for special situations.

### Deactivating libraries

`(library.unuse (cons <LIBRARY-NAME> (cons ... ())))` deactivates every library named in the list. Performing `library.unuse` on a library that is not active has no effect.

