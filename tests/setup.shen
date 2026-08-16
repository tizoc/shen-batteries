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

\\ Exercise checked custom patterns and same-file automatic registration.
(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "tests/fixtures/defpattern-input.shen")
      (load "tests/fixtures/pattern-input.shen")
      (tc Typechecking)))

(shen.x.features.cond-expand
  shen/scheme
    (let Typechecking (if (tc?) + -)
      (do (tc +)
          (load "tests/fixtures/with-exit-input.shen")
          (tc Typechecking)))
  true skip)

\\ Load the iterator cache independently while pattern matching is deferred.
(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "iter/mlist.shen")
      (tc Typechecking)))
