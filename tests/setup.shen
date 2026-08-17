(set *test-home-directory* (value *home-directory*))
(set *test-typechecking* (tc?))

(load "library.shen")
(shen.x.features.cond-expand
  shen/scheme (library.use [with-exit])
  true skip)
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
   seq/cexpr
   shendoc])
(library.use [box])

\\ Exercise checked custom patterns after automatic registration at a
\\ file boundary.
(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "tests/fixtures/defpattern-input.shen")
      (load "tests/fixtures/defpattern-use.shen")
      (load "tests/fixtures/pattern-input.shen")
      (tc Typechecking)))

(shen.x.features.cond-expand
  shen/scheme
    (let Typechecking (if (tc?) + -)
      (do (tc +)
          (load "tests/fixtures/with-exit-input.shen")
          (tc Typechecking)))
  true skip)

\\ `iter` is Shen/Scheme-only; retain portable cache coverage on other ports.
(shen.x.features.cond-expand
  shen/scheme (library.use [iter])
  true
    (let Typechecking (if (tc?) + -)
      (do (tc +)
          (load "iter/mlist.shen")
          (tc Typechecking))))
