\\ Ordinary comments are ignored.
\* Multiline comments are ignored too. *\

\\: = Fixture
\\:
\\: Intro.

(package sample [public external]

(datatype t
  X : A;
  __________
  X : (t A);)

\\: == API

\\: Documents a private typed function.
(define make
  { (t A) --> external }
  X -> X)

\\: Documents an external untyped function.
(define public
  X -> X)

\\: Documents a grammar rule.
(defcc <token>
  120 := x;)

\\: A detached note.

(set *fixture* true)

\\: A final note.
)
