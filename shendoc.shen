\\ Avoid `load` diagnostics so standard output contains only the document.
(map (/. Form (eval Form)) (read-file "shendoc/core.shen"))
(shendoc.main (value *argv*))
