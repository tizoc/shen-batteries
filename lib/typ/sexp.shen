\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 [typ/sexp.t]}

    [sexp] is the type inhabited by S-expressions.
*\

(datatype typ/sexp.t
    X : symbol;
    ______________
    X : atom;

    X : boolean;
    ______________
    X : atom;

    X : string;
    ______________
    X : atom;

    X : number;
    ______________
    X : atom;

    ______________
    [] : atom;

    X : atom;
    ______________
    X : sexp;

    [X | Y] : (list sexp);
    ========================
    [X | Y] : sexp;)