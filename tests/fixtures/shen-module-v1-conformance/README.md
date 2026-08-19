# `shen.module` v1 fixtures

These descriptors and sources are data for the reusable conformance suite in
[`tests/shen-module-v1-conformance.shen`](../../shen-module-v1-conformance.shen).
Do not load them as one module graph: the `descriptors`, `loading`,
`source-failure`, `cycle`, and `feature` cases must each run in a fresh Shen
process because version 1 loader state is permanent.

Run all five cases with `make test-module-conformance SHEN=/path/to/shen` from
the repository root. The
[`shen.module` conformance guide](../../../docs/shen-module-v1-conformance.md)
documents the expected outcome of every case and how to substitute another
loader implementation.

The feature case reserves
`shen-module-v1-conformance/absent-feature-91f8a7` as unavailable. A port under
test must not advertise that test-only feature.
