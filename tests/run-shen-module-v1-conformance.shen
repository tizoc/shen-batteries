(hush +)
(load "tests/harness.shen")
(load "library.shen")

\\ This runner intentionally starts in a fresh Shen process.  A loader under
\\ test must not already have a module home or loaded-module state.
(load "tests/shen-module-v1-conformance.shen")
(hush -)
(module-conformance.main (value *argv*))
