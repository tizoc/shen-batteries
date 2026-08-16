\\ Copyright (c) 2020 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Early exits
\\:
\\: `(with-return ReturnF Body)` binds `ReturnF` to a one-place function that when called
\\: interrupts the rest of the computation and returns from `with-exit` with the
\\: value passed as an argument.
\\:
\\: Example:
\\:
\\: [source,shen]
\\: (with-return Return (+ 3 4 (Return 10) 5))
\\: \\ Result: 10 : number
\\:
\\: `(with-break BreakF Body)` binds `BreakF` to a zero-place function that when called
\\: interrupts the rest of the computation and returns from `with-exit` with `(void)`.
\\: The result of a `with-break` expression is always the `void` object.
\\:
\\: Example:
\\:
\\: [source,shen]
\\: (with-break Break
\\:   (do (print "Hello ")
\\:       (Break)
\\:       (print "world!")))
\\: \\ Prints only "Hello "
\\:
\\: The variable bound by `with-return` and `with-break` is a syntactic construct, and not an actual function.
\\: Because of that some care is needed to avoid unexpected situations:
\\:
\\: * The behaviour of letting the function bound by either `with-return` or `with-break` escape the scope of the expression body is undefined.
\\: * The behaviour of rebinding the function to another name is undefined.
\\: * It is only valid to pass it around to another function is it is wrapped in a lambda.
\\:

(package with-exit [sexp void with-return with-break scm.call/1cc]

(datatype t
  Exit : (A --> B) >> Body : A;
  ____________________________
  (scm.call/1cc (lambda Exit Body)) : A;)

(datatype t-internal
  X : sexp;
  ____________________________
  (cons? X) : verified >> X : (list sexp);)

(define subst-return-application
  { symbol --> (sexp --> sexp) --> sexp --> sexp }
  Name F [Name Arg] -> (F Arg)
  Name F [Name | Rest] -> (error "Return function '~A' must be called with one argument, not ~A" Name (length Rest))
  Name F [let Name Value Body] -> [let Name (subst-return-application Name F Value) Body]
  Name F Z -> (map (/. W (subst-return-application Name F W)) Z)  where (cons? Z)
  _ _ Z -> Z)

(define subst-break-application
  { symbol --> sexp --> sexp --> sexp }
  Name Subst [Name] -> Subst
  Name Subst [Name | Rest] -> (error "Break function '~A' must be called with no arguments, not ~A" Name (length Rest))
  Name Subst [let Name Value Body] -> [let Name (subst-break-application Name Subst Value) Body]
  Name Subst Z -> (map (/. W (subst-break-application Name Subst W)) Z)  where (cons? Z)
  _ _ Z -> Z)

(defmacro macro
  [with-break BreakF Body]
    -> [[foreign scm.call/1cc]
        [lambda BreakF
          [do (subst-break-application BreakF [BreakF [void]] Body)
              [void]]]]

  [with-return ReturnF Body]
    -> [[foreign scm.call/1cc]
        [lambda ReturnF
          (subst-return-application ReturnF (/. R [ReturnF R]) Body)]])

(preclude [t-internal])

)
