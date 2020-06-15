\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(library.declare typ/or
  (loads tc+ "lib/typ/or.shen"))

(library.declare typ/unit
  (loads tc+ "lib/typ/unit.shen"))

(library.declare typ/verified-and-head
  (loads tc+ "lib/typ/verified-and-head.shen"))

(library.declare typ/verified-if
  (loads tc+ "lib/typ/verified-if.shen"))

(library.declare typ/verified-objects
  (loads tc+ "lib/typ/verified-objects.shen"))

(library.declare defpattern
  (loads "defpattern.shen"))

(library.declare dict
  (loads tc+ "dict.shen"))

(library.declare maybe
  (requires typ/unit defpattern)
  (loads tc+ "maybe.shen"))

(library.declare nullable
  (requires typ/unit defpattern)
  (loads tc+ "nullable.shen"))

(library.declare box
  (loads tc+ "box.shen"))

(library.declare lazy
  (requires defpattern box)
  (loads tc+ "lazy.shen"))

(library.declare lazy-pattern
  (requires lazy)
  (loads tc+ "lazy-pattern.shen"))

(library.declare seq
  (requires typ/unit typ/or maybe lazy)
  (loads tc+ "seq.shen"))

(library.declare seq-cexpr
  (requires typ/unit cexpr)
  (loads tc+ "seq-cexpr.shen"))

(library.declare cexpr
  (requires typ/unit)
  (loads tc+ "cexpr.shen"))

(library.declare let-match
  (loads "let-match.shen"))

(library.declare pipe-macro
  (loads "pipe-macro.shen"))
