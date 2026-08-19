\\ Avoid `load` diagnostics so standard output contains only the document.
(let Hush (if (hush?) + -)
  (do (trap-error
       (do (hush +)
           (load "shendoc/core.shen")
           (hush Hush))
       (/. E (do (hush Hush)
                 (simple-error (error-to-string E)))))
      (shendoc.main (value *argv*))))
