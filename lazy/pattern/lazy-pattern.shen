\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Lazy patterns
\\:
\\: Requiring `(library.use [lazy/pattern])` adds `(freeze Pattern)` to Shen's
\\: function patterns. Matching forces the lazy value with `thaw`, then matches
\\: `Pattern` against the result.
\\:
\\: == Syntax
\\:
\\: [source,shen]
\\: ----
\\: (define force
\\:   { (lazy A) --> A }
\\:   (freeze X) -> X)
\\:
\\: (force (freeze (+ 20 22)))
\\: \\ Result: 42
\\: ----
\\:
\\: A match thaws its input each time it is attempted. Use `lazy.memo` when
\\: the same delayed computation may be matched or thawed repeatedly and its
\\: successful result should be evaluated only once.

(package lazy-pattern [defpattern]

(datatype t
  X : A;
  =================================================
  (freeze X) : (lazy A);)

(defpattern lazy-pattern.pattern-handler
  Self Is? Assign [freeze X] -> (do (Assign X [thaw Self])
                                    handled))

)
