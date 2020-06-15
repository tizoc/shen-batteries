\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 [typ/sexp.t]}

    [sexp] is the type inhabited by S-expressions.
*\

(datatype typ/sexp.t
    X : symbol;
    ______________
    X : (mode atom -);

    X : boolean;
    ______________
    X :(mode  atom -);

    X : string;
    ______________
    X : (mode atom -);

    X : number;
    ______________
    X : (mode atom -);

    ______________
    [] : (mode atom -);

    X : atom;
    ______________
    X : (mode sexp -);

    [X | Y] : (list sexp);
    ========================
    [X | Y] : (mode sexp -);)