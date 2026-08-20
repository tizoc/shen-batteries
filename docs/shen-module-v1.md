# `shen.module` version 1

This document specifies the portable descriptor format and loading semantics
implemented by [`library.shen`](../library.shen). The key words **MUST**,
**MUST NOT**, **SHOULD**, and **MAY** describe conformance requirements.

Version 1 describes an already-present tree of named modules. It does not
define package discovery, installation, dependency versions, source exports,
separate compilation phases, or unloading.

## Descriptor grammar

A module descriptor is a Shen source file containing exactly one top-level
form. Its abstract grammar is:

```text
descriptor       ::= (shen.module field*)
field            ::= (version 1)
                   | (name symbol)
                   | (sources source-group+)
                   | (requires symbol*)
                   | (requires-features symbol*)
                   | (extension symbol sexp*)
source-group     ::= tc-mode string+
tc-mode          ::= tc+ | tc-
```

`version`, `name`, and `sources` are required and MUST each occur exactly once.
`requires` and `requires-features` are optional, default to empty lists, and
MUST occur at most once. Any number of `extension` fields may occur, but their
identifying symbols MUST be unique within the descriptor.

Fields may appear in any order. Standard fields not listed above, malformed
fields, and duplicate standard fields are errors. An extension body is an
opaque sequence of zero or more S-expressions; parsing it does not evaluate it.
The written order of dependencies, sources, required features, and extensions
is preserved.

For example:

```shen
(shen.module
  (version 1)
  (name example/tools)
  (requires box maybe)
  (requires-features host/feature)
  (sources tc+ "tools/types.shen" "tools/api.shen"
           tc- "tools/macros.shen")
  (extension example/host
    (mode compiled)))
```

## Names and descriptor locations

A module name and every entry in `requires` or `requires-features` MUST be a
Shen symbol. Version 1 adds no dependency version constraint or aliasing.

Given a configured module home `H`, the descriptor for module `M` is read from
`H/<printed-M>.shenmod`. Slash-separated symbols therefore form nested module
names. The `name` field MUST equal the name by which the descriptor was
requested.

The directory containing that descriptor is the base directory for its source
files. For example, `seq/cexpr.shenmod` declares module `seq/cexpr`, and a
source named `cexpr/seq-cexpr.shen` resolves beneath the `seq` directory.

Version 1 does not otherwise constrain the spelling of module symbols. A
distribution or host MAY impose additional safe-name rules before mapping an
untrusted module name to a filesystem path.

## Sources and typechecking modes

Every `sources` field contains at least one source. A `tc+` or `tc-` marker
starts a group and MUST be followed by one or more source strings. Its mode
applies to every following source until the next marker. A filename before the
first mode, a mode with no following filename, and a non-string filename are
errors.

Sources are loaded in their written order. Immediately before loading each
source, the loader enables typechecking for `tc+` or disables it for `tc-`.
The typechecking state that was active on entry to `library.use` MUST be
restored after the complete request, both on success and on error.

All dependencies in `requires` are ordinary module dependencies. Version 1
does not distinguish runtime and compile-time requirements.

## Portable source paths

A portable source path MUST:

- be a non-empty string;
- not begin with `/`;
- contain no backslash (`\`); and
- not have a colon (`:`) as its second character.

Forward slash (`/`) is the portable directory separator. These rules reject
absolute POSIX paths, Windows drive-prefixed paths such as `C:/file.shen`, and
Windows separators. Colons in other positions are not rejected. The version 1
reference parser does not separately normalize or reject `.` and `..` path
components; authors SHOULD avoid them and keep sources within the descriptor's
directory tree.

While reading a descriptor or loading its sources, a loader may temporarily
change Shen's `*home-directory*` so relative loads resolve correctly. It MUST
restore the caller's original home directory on success and on error.

## Dependencies and load order

For each requested module, a loader performs these operations:

1. If the module is already loaded, return without rereading it.
2. Read and validate its descriptor, including the requested-name check.
3. Check all declared feature requirements.
4. Recursively load `requires` entries from left to right.
5. Load the module's sources from left to right in their declared modes.
6. Mark the module loaded only after every source succeeds.

Top-level names passed to `library.use` are processed from left to right as
well. Each successfully loaded module is loaded at most once for the lifetime
of that loader state. A dependency cycle is an error.

Loading is not transactional. If a later dependency or source fails, modules
that completed earlier remain loaded, and effects of source forms evaluated
before the failure are not rolled back. The failing module itself is not
marked loaded.

## Required features

`requires-features` names capabilities supplied by the current Shen port or
runtime. Every listed feature MUST be present before dependency or source
loading begins; otherwise loading that module fails. Features are checked in
their written order, although all of them are required.

If a host exposes no feature-discovery operation, its available feature set is
empty. Feature names and their meanings are outside this specification. A
feature requirement is not a module dependency and does not cause any module
to be loaded.

## Extensions

An extension is identified by a symbol and contains an opaque body:

```shen
(extension shen/scheme
  (mode sealed)
  (exports example.answer))
```

The portable loader validates the extension envelope and uniqueness of its
identifier, but otherwise ignores it. A host-specific loader MAY interpret an
extension it owns. Loaders MUST accept and ignore extension identifiers they
do not understand; an unknown extension does not create a dependency or a
feature requirement. Extension identifiers SHOULD be namespaced to their
owner.

## Validation

A conforming loader rejects a descriptor when any of the following holds:

- the file contains zero or more than one top-level form;
- the form is not headed by `shen.module`;
- a required field is absent or the version is not exactly `1`;
- a standard field is duplicated, unknown, or malformed;
- `name`, a dependency, a required feature, or an extension identifier is not
  a symbol;
- extension identifiers are duplicated;
- `sources` is empty or violates its mode, string, or path rules; or
- the declared name differs from the requested module name.

Missing files, unavailable features, dependency cycles, and errors raised
while loading source files are load errors rather than descriptor-shape
errors. Exact diagnostic text is implementation-defined, but loading MUST stop
at the failing operation.

## Loader state and public operations

The reference loader initializes its module home from Shen's current
`*home-directory*`. `library.set-home` changes that root only while no module
has completed loading; after that it is an error. `library.use` loads a list of
module names with the ordering and state rules above. There is no unload
operation, and loading an already-loaded name has no effect.

`library.read-module` reads and validates one descriptor and checks its name,
but does not check features or load dependencies or sources.
`library.module-requires` returns the parsed direct dependency list. The exact
internal representation of a parsed declaration is an implementation detail
and is not part of version 1.

The reference loader also supports two public introspection operations:
`library.module-required-features` returns a parsed declaration's direct
feature requirements, and `library.current-features` returns the features
advertised by the running port. These conveniences are not required of another
version 1 loader or its conformance adapter.

## Conformance fixtures

The repository includes a reusable, port-neutral fixture suite for these
requirements. See the
[`shen.module` version 1 conformance guide](shen-module-v1-conformance.md) for
the covered behaviors, expected outcomes, and instructions for running either
the reference loader or another implementation.
