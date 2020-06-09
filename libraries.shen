\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(library.declare t
  (loads tc+ "t.shen"))

(library.declare dict
  (loads tc+ "dict.shen"))

(library.declare maybe
  (requires t)
  (provides-pattern-handlers maybe.pattern-handler)
  (loads tc+ "maybe.shen"))

(library.declare nullable
  (requires t)
  (provides-pattern-handlers nullable.pattern-handler)
  (loads tc+ "nullable.shen"))

(library.declare box
  (loads tc+ "box.shen"))

(library.declare lazy
  (requires t box)
  (loads tc+ "lazy.shen"))

(library.declare lazy-pattern
  (requires lazy)
  (provides-pattern-handlers lazy.pattern-handler)
  (loads tc+ "lazy-pattern.shen"))

(library.declare seq
  (requires t maybe lazy)
  (loads tc+ "seq.shen"))

(library.declare seq-cexpr
  (requires t cexpr)
  (loads tc+ "seq-cexpr.shen"))

(library.declare cexpr
  (requires t)
  (loads tc+ "cexpr.shen"))

(library.declare let-match
  (loads "let-match.shen"))

(library.declare pipe-macro
  (loads "pipe-macro.shen"))
