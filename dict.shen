\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 Dictionaries}

    Public API and types for internal Shen dictionary data structure.

    TODO
*\

(package dict []

(datatype t-internal
  ________________________
  (shen.dict X) : (dict.t Key Value);

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

\** {2 API} *\

\** {3 Construction} *\

\** [(dict.make SizeHint)] creates a new dictionary. [SizeHint] is a size hint for the underlying implementation,
    the recommended value is equal to the expected amount values this dict will hold. *\
(define dict.make
  { number --> (dict.t Key Value) }
  SizeHint -> (shen.dict SizeHint))

\** {3 Predicates} *\

\** [(dict.is? X)] returns [true] if [X] is a dictionary, and [false] otherwise. *\
(define dict.is?
  { A --> boolean }
  Value -> (shen.dict? Value))

\** {3 Access} *\

\** [(dict.get Dict Key)] returns the value in [Dict] associated with [Key]. Raises an error if no value associated with [Key] is found. *\
(define dict.get
  { (dict.t Key Value) --> Key --> Value }
  Dict Key -> (shen.<-dict Dict Key))

\** {3 Modification} *\

\** [(dict.set Dict Key Value)] associates [Value] with [Key] in [Dict]. *\
(define dict.set
  { (dict.t Key Value) --> Key --> Value --> Value }
  Dict Key Value -> (shen.dict-> Dict Key Value))

\** [(dict.delete Dict Key)] deletes the value associated with [Key] in [Dict]. *\
(define dict.delete
  { (dict.t Key Value) --> Key --> Key }
  Dict Key -> (shen.dict-rm Dict Key))

\** {3 Traversal} *\

\** [(dict.fold F Dict Accum)] calls [(F Key Value Accum)] for each association in [Dict].
    The return value of each call to [F] is used as [Accum] in the next call.
    Returns the last result of calling [F]. *\
(define dict.fold
  { (Key --> Value --> A --> A) --> (dict.t Key Value) --> A --> A }
  F Dict Seed -> (shen.dict-fold F Dict Seed))

\** [(dict.keys Dict)] returns a list of all keys in [Dict]. *\
(define dict.keys
  { (dict.t Key Value) --> (list Key) }
  Dict -> (shen.dict-keys Dict))

\** [(dict.values Dict)] returns a list of all values in [Dict]. *\
(define dict.values
  { (dict.t Key Value) --> (list Value) }
  Dict -> (shen.dict-values Dict))

(preclude [t-internal])

)