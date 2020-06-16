\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Dictionaries
\\:
\\: Public API and types for internal Shen dictionary data structure.

(package dict []

(datatype t-internal
  ________________________
  (shen.dict X) : (dict.t K V);

  ________________________
  (shen.dict? X) : boolean;

  ________________________
  (shen.dict-> D K V) : Value;

  ________________________
  (shen.<-dict D K) : Value;

  ________________________
  (shen.dict-rm D K) : Key;

  ________________________
  (shen.dict-fold F Dict Seed) : A;

  ________________________
  (shen.dict-keys D) : (list Key);

  ________________________
  (shen.dict-values D) : (list Value);)

\\: == API

\\: === Construction

\\: `(dict.make SizeHint)` creates a new dictionary. `SizeHint` is a size
\\: hint for the underlying implementation, the recommended value is equal to the
\\: expected amount values this dict will hold.
(define dict.make
  { number --> (dict.t K V) }
  SizeHint -> (shen.dict SizeHint))

\\: === Predicates

\\: `(dict.is? X)` returns `true` if `X` is a dictionary, and `false` otherwise.
(define dict.is?
  { A --> boolean }
  Value -> (shen.dict? Value))

\\: === Access

\\: `(dict.get Dict Key)` returns the value in `Dict` associated with `Key`.
\\: Raises an error if no value associated with `Key` is found.
(define dict.get
  { (dict.t K V) --> K --> V }
  Dict Key -> (shen.<-dict Dict Key))

\\: === Modification

\\: `(dict.set Dict Key Value)` associates `Value` with `Key` in `Dict`.
(define dict.set
  { (dict.t K V) --> K --> V --> V }
  Dict Key Value -> (shen.dict-> Dict Key Value))

\\: `(dict.delete Dict Key)` deletes the value associated with `Key` in `Dict`.
(define dict.delete
  { (dict.t K V) --> Key --> Key }
  Dict Key -> (shen.dict-rm Dict Key))

\\: === Traversal

\\: `(dict.fold F Dict Accum)` calls `(F Key Value Accum)` for each association in `Dict`.
\\: The return value of each call to `F` is used as `Accum` in the next call.
\\: Returns the last result of calling `F`.
(define dict.fold
  { (K --> V --> A --> A) --> (dict.t K V) --> A --> A }
  F Dict Seed -> (shen.dict-fold F Dict Seed))

\\: `(dict.keys Dict)` returns a list of all keys in `Dict`.
(define dict.keys
  { (dict.t K V) --> (list K) }
  Dict -> (shen.dict-keys Dict))

\\: `(dict.values Dict)` returns a list of all values in `Dict`.
(define dict.values
  { (dict.t K V) --> (list V) }
  Dict -> (shen.dict-values Dict))

(preclude [t-internal])

)