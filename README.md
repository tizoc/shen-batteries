Shen Batteries
==============

Collection of libraries for Shen.

Loading libraries
-----------------

Load the module loader, point it at this repository, and use one or more
libraries:

```shen
(load "library.shen")
(library.set-home "/path/to/shen-batteries")
(library.use [box lazy])
```

`library.use` loads dependencies and sources once. Loading is permanent for the
current Shen image; there is no unload operation. Set the module home before
the first call to `library.use`; it cannot change after modules are loaded.
See the [loader API](docs/library.adoc) for the exact behavior.

Dependencies are loaded automatically, so loading an adapter is enough to load
its base library:

```shen
(library.use [seq/cexpr])
```

Modules use the portable `shen.module` version 1 format. Descriptors are found
at `<module-root>/<module-name>.shenmod`, and their source paths are relative to
the descriptor. Port-specific settings belong in namespaced `extension` fields
and are ignored by the portable source loader. See the normative
[`shen.module` version 1 specification](docs/shen-module-v1.md).

Module catalog
--------------

The requirements below are the direct requirements declared by each module;
`library.use` resolves their transitive dependencies. A module's feature
requirements are checked against the current Shen implementation before that
module's dependencies or sources are loaded. Each reference is generated from
the module's source documentation unless it is the port-neutral module-format
specification.

| Module | Purpose | Declared requirements | Reference |
| --- | --- | --- | --- |
| `box` | Mutable references | `typ/void` | [API](docs/box.adoc) |
| `cexpr` | Generic computation-expression parser and expander | `typ/sexp` | [Guide and API](docs/cexpr.adoc) |
| `defpattern` | Definition helper for programmable pattern-matching handlers | `typ/sexp` | [API](docs/defpattern.adoc) |
| `dict` | Typed public API for Shen dictionaries | — | [API](docs/dict.adoc) |
| `features` | Feature registration, discovery, and conditional expansion | — | [API](docs/features.adoc) |
| `iter` | Push-based iterators | `typ/void`, `maybe`, `box`, `let-match`, `with-exit` | [Guide](docs/choosing-seq-or-iter.adoc), [API](docs/iter.adoc) |
| `lazy` | Memoization for frozen computations | `box` | [API](docs/lazy.adoc) |
| `lazy/pattern` | Programmable matching of frozen computations | `defpattern`, `lazy` | [API](docs/lazy/pattern.adoc) |
| `let-match` | List and tuple destructuring in `let` bindings | — | [API](docs/let-match.adoc) |
| `maybe` | Tagged optional values and programmable patterns | `typ/void`, `defpattern` | [API](docs/maybe.adoc) |
| `maybe/cexpr` | `maybe.do` computation-expression frontend | `maybe`, `typ/sexp`, `cexpr` | [API](docs/maybe/cexpr.adoc) |
| `nullable` | Identity-represented nullable values and programmable patterns | `defpattern` | [API](docs/nullable.adoc) |
| `pipe-macro` | Thread-first, thread-last, and `doto` macros | — | [API](docs/pipe-macro.adoc) |
| `seq` | Pull-based lazy sequences | `typ/void`, `typ/or`, `maybe`, `lazy` | [Guide](docs/choosing-seq-or-iter.adoc), [API](docs/seq.adoc) |
| `seq/cexpr` | `seq.do` computation-expression frontend | `seq`, `typ/sexp`, `cexpr` | [API](docs/seq/cexpr.adoc) |
| `seq/dict` | Snapshot conversions between sequences and dictionaries | `seq`, `dict` | [API](docs/seq/dict.adoc) |
| `shendoc` | AsciiDoc generator for Shen source comments | — | [API](docs/shendoc.adoc) |
| `typ/or` | Union type rule | — | [API](docs/typ/or.adoc) |
| `typ/sexp` | S-expression type | — | [API](docs/typ/sexp.adoc) |
| `typ/verified-and-head` | Type refinement from the head of `and` | — | [API](docs/typ/verified-and-head.adoc) |
| `typ/verified-if` | Type refinement in the true branch of `if` | — | [API](docs/typ/verified-if.adoc) |
| `typ/verified-objects` | Predicate-based type-refinement rules | — | [API](docs/typ/verified-objects.adoc) |
| `typ/void` | Type and value for discarded results | — | [API](docs/typ/void.adoc) |
| `with-exit` | Portable early-return and early-break forms | `box`, `features`, `maybe`, `typ/void`, `typ/sexp` | [API](docs/with-exit.adoc) |

No production module currently declares a port feature requirement.

Testing
-------

Provide the Shen executable explicitly:

```sh
make test SHEN=/absolute/path/to/shen
```

The current reference runtime is Shen 41.3 from Shen kernel `master` on
Shen/Scheme `master` after 0.46. The Shen/Scheme 0.46 release predates the
required programmable-pattern typechecking and dictionary traversal fixes.

The same Shen/Scheme development build can also compile and load the native
module smoke graph:

```sh
make test-native SHEN=/absolute/path/to/shen-scheme
```

This target is intentionally separate from the portable source-loader tests.

Regenerate the tracked API documentation from source comments with:

```sh
make docs SHEN=/absolute/path/to/shen
```

License
-------

Copyright © 2019 Bruno Deferrari under [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause).
