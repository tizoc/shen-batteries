\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = `typ/sexp.t`
\\:
\\: This datatype defines the recursive `sexp` and `atom` types. A `sexp` is an
\\: `atom` or a proper list whose elements are themselves `sexp` values. An
\\: `atom` is a symbol, boolean, string, number, or `[]`.
\\:
\\: [source,shen]
\\: ----
\\: (library.use [typ/sexp])
\\:
\\: (define call-form
\\:   { symbol --> (list sexp) --> sexp }
\\:   Function Arguments -> [Function | Arguments])
\\: ----
\\:
\\: This is the boundary for data represented as ordinary S-expressions, not a
\\: type for every Shen value. Improper lists, vectors, functions, streams, and
\\: native host objects do not inhabit `sexp` through these rules. `[]` is
\\: accepted both as the empty proper list and explicitly as an `atom`.

(datatype typ/sexp.t
  [X | Y] : (list sexp);
  ========================
  [X | Y] : (- sexp);

  X : (list sexp);
  ______________
  X : (- sexp);

  X : atom;
  ______________
  X : (- sexp);

  X : symbol;
  ______________
  X : (- atom);

  X : boolean;
  ______________
  X : (- atom);

  X : string;
  ______________
  X : (- atom);

  X : number;
  ______________
  X : (- atom);

  ______________
  [] : (- atom);)
