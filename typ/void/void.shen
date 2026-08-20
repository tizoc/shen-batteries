\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = `void`
\\:
\\: `void` is the result type for computations whose value should be ignored.
\\: `typ/void.t` admits every otherwise-typable expression as `void`, so `void`
\\: is a discard type rather than a singleton type or a claim that the result is
\\: equal to `(void)`.
\\:
\\: [source,shen]
\\: ----
\\: (library.use [typ/void])
\\:
\\: (define discard-number
\\:   { number --> void }
\\:   X -> (+ X 1))
\\: ----
\\:
\\: `discard-number` still returns the computed number at runtime; its declared
\\: result type hides that concrete type and expresses that callers are expected
\\: to ignore the value. Use `void` only in APIs where the result is intentionally
\\: discarded.
\\:
\\: `(void)` supplies a conventional ignored result. On Shen/Scheme it returns
\\: Scheme's native void value through `(foreign scm.void)`; on ports without the
\\: `shen/scheme` feature it returns the fallback symbol `-void-`. Portable code
\\: should not inspect, compare, or otherwise depend on this representation.
\\:
\\: The precluded `typ/void.internal-t` theory exists only so the port-specific
\\: implementation of `(void)` can be checked while this module is loaded.

(datatype typ/void.internal-t
  ____________________
  X : (- void);)

(datatype typ/void.t
  X : A;
  ____________________
  X : (- void);)

(define void
  { --> void}
  -> (shen.x.features.cond-expand
       shen/scheme ((foreign scm.void))
       true -void-))

(preclude [typ/void.internal-t])
