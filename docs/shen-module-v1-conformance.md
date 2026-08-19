# `shen.module` version 1 conformance suite

The checked fixtures under
[`tests/fixtures/shen-module-v1-conformance`](../tests/fixtures/shen-module-v1-conformance)
exercise the portable behavior specified by
[`shen.module` version 1](shen-module-v1.md). They contain only Shen source and
version 1 descriptors; none of the success cases depends on a Shen/Scheme
extension or native compilation.

## Run the reference loader on another port

From the repository root, provide that port's Shen executable:

```sh
make test-module-conformance SHEN=/absolute/path/to/shen
```

The target starts five fresh Shen processes. Loader state is permanent, so
fresh processes are part of the test protocol rather than an optimization.
They keep descriptor validation, successful loading, partial failure, cycle,
and feature-rejection state independent.

The cases and their expected outcomes are:

| Case | Expected outcome |
| --- | --- |
| `descriptors` | Valid declarations parse without loading code; representative malformed declarations and non-portable paths raise errors; descriptor reads restore `*home-directory*` without locking module home. |
| `loading` | Dependencies, top-level requests, and sources run left to right; shared dependencies and repeated requests run once; `tc+` checks types while `tc-` permits unchecked code; extensions are ignored; caller state and per-source descriptor-relative resolution are preserved. |
| `source-failure` | Missing and failing sources raise errors; completed dependencies and earlier source effects remain; the failed module is retryable; later top-level requests do not run; home and typechecking state are restored. |
| `cycle` | A dependency cycle raises an error before either cyclic module executes a source; no completed module means module home remains changeable. |
| `feature` | An unavailable feature raises an error before that module's dependency or source executes; no completed module means module home remains changeable. |

Assertions deliberately compare behavior, not diagnostic strings. Version 1
leaves exact error wording implementation-defined.

The feature case uses
`shen-module-v1-conformance/absent-feature-91f8a7` as a reserved unavailable
feature name. A port running this suite must not advertise that test-only
feature.

## Test another loader implementation

The reusable suite is
[`tests/shen-module-v1-conformance.shen`](../tests/shen-module-v1-conformance.shen).
It expects these version 1 operations in the `library` package:

- `library.set-home`
- `library.use`
- `library.read-module`
- `library.module-requires`

The repository runner first loads `library.shen`, then the suite, and finally
dispatches the case named by its script argument. To test another loader,
copy the runner and replace only its loader form:

```shen
(hush +)
(load "tests/harness.shen")
(load "path/to/port-loader.shen")
(load "tests/shen-module-v1-conformance.shen")
(hush -)
(module-conformance.main (value *argv*))
```

Invoke that runner once for each of `descriptors`, `loading`,
`source-failure`, `cycle`, and `feature`, always from the repository root and
in a new process. If a port's command-line runner does not expose script
arguments through `*argv*`, use five small runners which call, for example,
`(module-conformance.main [runner "loading"])` directly. The fixtures and
suite logic themselves do not depend on argument handling.

An implementation may use a thin adapter to expose the four operations under
the expected names. Internal parsed-declaration representation is irrelevant;
only `library.module-requires` is used to observe a declaration.

## Scope

The suite covers the portable parser and runtime loading transitions most
likely to diverge between ports. It does not test package installation,
version solving, unloading, native compilation, host interpretation of
extension bodies, or exact diagnostics because version 1 specifies none of
those behaviors.
