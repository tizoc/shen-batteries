\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Feature selection
\\:
\\: Feature names describe facilities supplied by the current Shen port or by
\\: loaded libraries. This module exposes the current names and a convenient
\\: conditional-expansion form. Require it with `(library.use [features])`.
\\:
\\: == API
\\:
\\: === `features.cond`
\\:
\\: `(features.cond Condition Expression ...)` accepts alternating condition
\\: and expression forms and expands to the expression for the first satisfied
\\: condition. Selection happens during macro expansion, so only the selected
\\: expression remains for later compilation and evaluation. A condition may
\\: be a feature symbol,
\\: `(and Feature ...)`, `(or Feature ...)`, `(not Feature)`, or `true` for a
\\: final fallback. If no condition is satisfied, expansion raises an error.
\\:
\\: [source,shen]
\\: ----
\\: (features.cond
\\:   shen/scheme scheme-implementation
\\:   (or browser jvm) managed-implementation
\\:   true portable-implementation)
\\: ----

(defmacro features.macro
  [features.cond | Rest] -> [shen.x.features.cond-expand | Rest])

\\: `(features.add Name)` adds `Name` to the process-wide feature set if it is
\\: absent. Adding an existing name has no effect. The function returns the
\\: feature list as it was before the update; later conditional expansions can
\\: select a newly added feature.
(define features.add
  { symbol --> (list symbol) }
  Name -> (shen.x.features.add Name))

\\: `(features.current)` returns the process-wide list of feature names known
\\: to the running Shen implementation. Treat the result as a set: list order
\\: is not part of the portable interface.
(define features.current
  { --> (list symbol) }
  -> (shen.x.features.current))
