\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package seq [seq maybe maybe.some? maybe.nothing]

(datatype seq-internal

  __________________
  [] : (mode (node A) -);

  X : A;
  Seq : (seq A);
  ===================
  (cons X Seq) : (mode (node A) -);

  Node : (node A);
  __________________
  (freeze Node) : (mode (seq A) -);

  Seq : (seq A);
  __________________
  (thaw Seq) : (node A);)

\\ Creation

(define empty
  { --> (seq A) }
  -> (freeze []))

(define singleton
  { A --> (seq A) }
  X -> (freeze [X | (empty)]))

(define seq.cons
  { A --> (seq A) --> (seq A) }
  V S -> (freeze [V | S]))

(define snoc
  { (seq A) --> A --> (seq A) }
  S V -> (seq.append S (singleton V)))

(define make
  { number --> A --> (seq A) }
  0 _ -> (empty)
  N Elt -> (freeze [Elt | (make (- N 1) Elt)]))

(define init
  { (number --> A) --> (seq A) }
  F -> (init-h 0 F))

(define init-h
  { number --> (number --> A) --> (seq A) }
  N F -> (freeze [(F N) | (init-h (+ N 1) F)]))

(define seq.range-step
  { number --> number --> number --> (seq number) }
  Step Start End -> (seq.range-step-increasing-h Step Start End) where (> Step 0)
  Step Start End -> (seq.range-step-decreasing-h (- 0 Step) Start End) where (< Step 0)
  _ _ _ -> (error "seq.range-step called with Step=0"))

(define seq.range-step-increasing-h
  { number --> number --> number --> (seq number) }
  Step Start End -> (seq.empty) where (> Start End)
  Step Start End -> (freeze [Start | (seq.range-step-increasing-h Step (+ Start Step) End)]))

(define seq.range-step-decreasing-h
  { number --> number --> number --> (seq number) }
  Step Start End -> (seq.empty) where (< Start End)
  Step Start End -> (freeze [Start | (seq.range-step-decreasing-h Step (- Start Step) End)]))

(define seq.range
  { number --> number --> (seq number) }
  Start End -> (seq.range-step 1 Start End) where (>= End Start)
  Start End -> (seq.range-step -1 Start End))

\\ TODO: of-vector/string/dict

(define of-list
  { (list A) --> (seq A) }
  [] -> (empty)
  [X | Xs] -> (seq.cons X (of-list Xs)))

\\ TODO: to-vector/string/dict

(define to-list
  { (seq A) --> (list A) }
  S -> (to-list-h (thaw S)))

(define to-list-h
  { (node A) --> (list A) }
  [] -> []
  [X | Seq] -> [X | (to-list-h (thaw Seq))])

(define forever
  { (lazy A) --> (seq A) }
  L -> (freeze [(thaw L) | (forever L)]))

\\ Predicates

(define seq.empty?
  { (seq A) --> boolean}
  S -> (empty-node? (thaw S)))

(define empty-node?
  { (node A) --> boolean}
  [] -> true
  _ -> false)

\\ Accessors

(define node-head
  { (node A) --> A }
  [] -> (error "seq.head called on empty seq")
  [H | _] -> H)

(define node-tail
  { (node A) --> (seq A) }
  [] -> (error "seq.tail called on empty seq")
  [_ | T] -> T)

(define seq.head
  { (seq A) --> A }
  S -> (node-head (thaw S)))

(define seq.tail
  { (seq A) --> (seq A) }
  S -> (node-tail (thaw S)))

\\ Consumption

(define fold-left
  { (A --> B --> A) --> A --> (seq B) --> A }
  F Init S -> (fold-left-h F Init (thaw S)))

(define fold-left-h
  { (A --> B --> A) --> A --> (node B) --> A }
  _ Acc [] -> Acc
  F Acc [H | T] -> (fold-left-h F (F Acc H) (thaw T)))

(define for-each
  { (A --> Any) --> (seq A) --> (list unit) }
  F S -> (for-each-h F (thaw S)))

(define for-each-h
  { (A --> Any) --> (node A) --> (list unit)}
  _ [] -> []
  F [H | T] -> (do (F H) (for-each-h F (thaw T))))

(define equal?
  { (seq A) --> (seq A) --> boolean }
  S1 S2 -> (equal?-h (thaw S1) (thaw S2)))

(define equal?-h
  { (node A) --> (node A) --> boolean }
  [] [] -> true
  [X | XSeq] [X | YSeq] -> (equal?-h (thaw XSeq) (thaw YSeq))
  _ _ -> false)

(define equal-cmp?
  { (A --> B --> boolean) --> (seq A) --> (seq B) --> boolean }
  Cmp S1 S2 -> (equal-cmp?-h Cmp (thaw S1) (thaw S2)))

(define equal-cmp?-h
  { (A --> B --> boolean) --> (node A) --> (node B) --> boolean }
  _ [] [] -> true
  Cmp [X | XSeq] [Y | YSeq] -> (equal-cmp?-h Cmp (thaw XSeq) (thaw YSeq)) where (Cmp X Y)
  _ _ _ -> false)

(define for-all?
  { (A --> boolean) --> (seq A) --> boolean }
  F S -> (for-all?-h F (thaw S)))

(define for-all?-h
  { (A --> boolean) --> (node A) --> boolean }
  F [] -> true
  F [X | Seq] -> (for-all?-h F (thaw Seq)) where (F X)
  _ _ -> false)

(define exists?
  { (A --> boolean) --> (seq A) --> boolean }
  F S -> (exists?-h F (thaw S)))

(define exists?-h
  { (A --> boolean) --> (node A) --> boolean }
  F [] -> false
  F [X | _] -> true where (F X)
  F [X | Seq] -> (exists?-h F (thaw Seq)))

(define seq.element?
  { A --> (seq A) --> boolean }
  X S -> (exists? (= X) S))

(define element-cmp?
  { (A --> B --> boolean) --> A --> (seq B) --> boolean }
  Cmp X S -> (exists? (Cmp X) S))

(define find
  { (A --> boolean) --> (seq A) --> (maybe A) }
  F S -> (find-h F (thaw S)))

(define find-h
  { (A --> boolean) --> (node A) --> (maybe A) }
  _ [] -> (maybe.nothing)
  F [X | Seq] -> (if (F X) X (find-h F (thaw Seq))))

(define find-map
  { (A --> (maybe B)) --> (seq A) --> (maybe B) }
  F S -> (find-map-h F (thaw S)))

(define find-map-h
  { (A --> (maybe B)) --> (node A) --> (maybe B) }
  _ [] -> (maybe.nothing)
  F [X | Seq] -> (let Result (F X)
                   (if (maybe.some? Result)
                       Result
                       (find-map-h F (thaw Seq)))))

\\ Transformation

(define seq.map
  { (A --> B) --> (seq A) --> (seq B) }
  F S -> (freeze (map-h F (thaw S))))

(define map-h
  { (A --> B) --> (node A) --> (node B)}
  _ [] -> []
  F [H | T] -> [(F H) | (freeze (map-h F (thaw T)))])

(define filter
  { (A --> boolean) --> (seq A) --> (seq A) }
  F S -> (freeze (filter-h F (thaw S))))

(define filter-h
  { (A --> boolean) --> (node A) --> (node A)}
  _ [] -> []
  F [H | T] -> [H | (freeze (filter-h F (thaw T)))]
      where (F H)
  F [H | T] -> (filter-h F (thaw T)))

(define filter-map
  { (A --> (maybe B)) --> (seq A) --> (seq B) }
  F S -> (filter-map-h F (thaw S)))

(define filter-map-h
  { (A --> (maybe B)) --> (node A) --> (seq B) }
  F [V | Seq] -> (filter-map-hh F (F V) Seq))

(define filter-map-hh
  { (A --> (maybe B)) --> (maybe B) --> (seq A) --> (seq B) }
  F V Seq -> (freeze [V | (filter-map F Seq)]) where (maybe.some? V)
  F _ Seq -> (filter-map F Seq))

(define flat-map
  { (A --> (seq B)) --> (seq A) --> (seq B) }
  F A -> (flat-map-h F (thaw A)))

(define flat-map-h
  { (A --> (seq B)) --> (node A) --> (seq B) }
  _ [] -> (freeze [])
  F [X | Next] -> (flat-map-append F (F X) Next))

(define flat-map-append
  { (A --> (seq B)) --> (seq B) --> (seq A) --> (seq B) }
  F S Tail -> (flat-map-append-h F (thaw S) Tail))

(define flat-map-append-h
  { (A --> (seq B)) --> (node B) --> (seq A) --> (seq B) }
  F [] Tail -> (flat-map F Tail)
  F [X | Next] Tail -> (freeze [X | (flat-map-append F Next Tail)]))

(define seq.append
  { (seq A) --> (seq A) --> (seq A) }
  A B -> (append-h (thaw A) B))

(define append-h
  { (node A) --> (seq A) --> (seq A) }
  [] B -> B
  [H | T] B -> (freeze [H | (seq.append T B)]))

(define seq.concat
  { (list (seq A)) --> (seq A)}
  [] -> (empty)
  [S | Ss] -> (seq.append S (seq.concat Ss)))

(define flatten
  { (seq (seq A)) --> (seq A)}
  S -> (flatten-h (thaw S)))

(define flatten-h
  { (node (seq A)) --> (seq A)}
  [] -> (empty)
  [S | Ss] -> (seq.append S (seq.flatten-h (thaw Ss))))

(define cycle
  { (seq A) --> (seq A) }
  S -> (freeze (thaw (seq.append S (cycle S)))))

(define take
  { number --> (seq A) --> (seq A) }
  0 _ -> (empty)
  N S -> (freeze
          (thaw (seq.cons (seq.head S)
                          (take (- N 1) (seq.tail S))))))

(define drop
  { number --> (seq A) --> (seq A) }
  0 S -> S
  N S -> (freeze (thaw (drop (- N 1) (seq.tail S)))))

(define take-while
  { (A --> boolean) --> (seq A) --> (seq A) }
  F S -> (empty) where (not (F (seq.head S)))
  F S -> (freeze
          (thaw (seq.cons (seq.head S)
                          (take-while F (seq.tail S))))))

(define drop-while
  { (A --> boolean) --> (seq A) --> (seq A) }
  F S -> S where (not (F (seq.head S)))
  F S -> (freeze (thaw (drop-while F (seq.tail S)))))

(define zip-with
  { (A --> B --> C) --> (seq A) --> (seq B) --> (seq C) }
  Cons S1 S2 -> (zip-with-h Cons (thaw S1) (thaw S2)))

(define zip-with-h
  { (A --> B --> C) --> (node A) --> (node B) --> (seq C) }
  _ [] _ -> (empty)
  _ _ [] -> (empty)
  Cons [X | XSeq] [Y | YSeq] -> (freeze [(Cons X Y) | (zip-with-h Cons (thaw XSeq) (thaw YSeq))]))

(define zip
  { (seq A) --> (seq B) --> (seq (A * B)) }
  S1 S2 -> (zip-h (thaw S1) (thaw S2)))

(define zip-h
  { (node A) --> (node B) --> (seq (A * B))}
  [] _ -> (empty)
  _ [] -> (empty)
  [X | XSeq] [Y | YSeq] -> (freeze [(@p X Y) | (zip-h (thaw XSeq) (thaw YSeq))]))

(define unzip
  { (seq (A * B)) --> ((seq A) * (seq B)) }
  S -> (@p (seq.map (function fst) S)
           (seq.map (function snd) S)))

\\ TODO: chunks { number -> (seq A) -> (seq (vector A)) }

(preclude [seq-internal])

)
