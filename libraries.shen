\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(library.declare typ/or
  (loads tc+ "lib/typ/or.shen"))

(library.declare typ/sexp
  (loads tc+ "lib/typ/sexp.shen"))

(library.declare typ/verified-and-head
  (loads tc+ "lib/typ/verified-and-head.shen"))

(library.declare typ/verified-if
  (loads tc+ "lib/typ/verified-if.shen"))

(library.declare typ/verified-objects
  (loads tc+ "lib/typ/verified-objects.shen"))

(library.declare typ/void
  (loads tc+ "lib/typ/void.shen"))

(library.declare defpattern
  (requires typ/sexp typ/void)
  (loads "defpattern.shen"))

(library.declare dict
  (loads tc+ "dict.shen"))

(library.declare maybe
  (requires typ/void typ/sexp defpattern)
  (loads tc+ "maybe.shen"))

(library.declare nullable
  (requires typ/void typ/sexp defpattern)
  (loads tc+ "nullable.shen"))

(library.declare box
  (requires typ/void)
  (loads tc+ "box.shen"))

(library.declare lazy
  (requires box)
  (loads tc+ "lazy.shen"))

(library.declare lazy-pattern
  (requires typ/void typ/sexp defpattern lazy)
  (loads tc+ "lazy-pattern.shen"))

(library.declare seq
  (requires typ/void typ/or maybe lazy)
  (loads tc+ "seq.shen"))

(library.declare iter
  (requires typ/void maybe box let-match with-exit)
  (loads tc+ "iter.shen"))

(library.declare seq-cexpr
  (requires typ/sexp cexpr)
  (loads tc+ "seq-cexpr.shen"))

(library.declare cexpr
  (requires typ/sexp typ/void let-match)
  (loads tc+ "cexpr.shen"))

(library.declare let-match
  (loads "let-match.shen"))

(library.declare pipe-macro
  (loads "pipe-macro.shen"))

(library.declare features
  (loads tc+ "features.shen"))

(library.declare with-exit
  (requires box features maybe typ/void typ/sexp)
  (loads tc+ "with-exit.shen"))
