(set *test-home-directory* (value *home-directory*))
(set *test-typechecking* (tc?))

(load "library.shen")
(library.use [with-exit])
(library.use
  [features
   box
   lazy
   maybe
   nullable
   lazy/pattern
   dict
   typ/or
   typ/sexp
   typ/verified-objects
   typ/verified-and-head
   defpattern
   let-match
   pipe-macro
   cexpr
   maybe/cexpr
   seq/dict
   seq/cexpr
   shendoc])
(library.use [box])

\\ Exercise checked computation-expression builders after their frontend
\\ declarations have become active at a file boundary.
(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "tests/fixtures/cexpr-input.shen")
      (load "tests/fixtures/cexpr-package-input.shen")
      (tc Typechecking)))

\\ Exercise checked custom patterns after automatic registration at a
\\ file boundary.
(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "tests/fixtures/defpattern-input.shen")
      (load "tests/fixtures/defpattern-use.shen")
      (load "tests/fixtures/pattern-input.shen")
      (tc Typechecking)))

(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "tests/fixtures/with-exit-input.shen")
      (tc Typechecking)))

\\ Exercise pipe syntax in checked definitions and inside a package whose
\\ external list keeps the globally installed macro names unqualified.
(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "tests/fixtures/pipe-macro-input.shen")
      (tc Typechecking)))

(library.use [iter])
