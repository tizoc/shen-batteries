(load "tests/harness.shen")

\\ Select the portable implementations before their defining sources are
\\ macro-expanded. This runs in its own process so the feature change cannot
\\ affect the main suite.
(shen.x.features.initialise
  (remove shen/scheme.records
    (remove shen/scheme (shen.x.features.current))))

(test.assert-equal
  "portable fallback test disables shen/scheme"
  false
  (element? shen/scheme (shen.x.features.current)))

(test.assert-equal
  "portable fallback test disables native record support"
  false
  (element? shen/scheme.records (shen.x.features.current)))

(load "library.shen")
(library.use [iter])
(library.use [record])
(set *record-backend*
     (/. Declaration (record.portable-backend Declaration)))

(let Typechecking (if (tc?) + -)
  (do (tc +)
      (load "tests/fixtures/with-exit-input.shen")
      (load "tests/fixtures/record-declarations.shen")
      (tc Typechecking)))

\\ Load assertions only after the selected macro implementations and generated
\\ record API have been installed at the preceding file boundary.
(load "tests/portable-fallback.shen")

(test.finish)
