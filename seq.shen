\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package seq [unit maybe.t maybe.some? maybe.get @none @some mfreeze]

(datatype seq-internal

  __________________
  [] : (mode (node A) -);

  X : A;
  Seq : (t A);
  ===================
  (cons X Seq) : (mode (node A) -);

  Node : (node A);
  __________________
  (freeze Node) : (mode (t A) -);

  Node : (node A);
  __________________
  (mfreeze Node) : (mode (t A) -);

  Seq : (t A);
  __________________
  (thaw Seq) : (node A);)

\\ Creation

(define empty
  { --> (t A) }
  -> (freeze []))

(define singleton
  { A --> (t A) }
  X -> (mfreeze [X | (empty)]))

(define seq.cons
  { A --> (t A) --> (t A) }
  V S -> (mfreeze [V | S]))

(define snoc
  { (t A) --> A --> (t A) }
  S V -> (seq.append S (singleton V)))

(define make
  { number --> A --> (t A) }
  0 _ -> (empty)
  N Elt -> (mfreeze [Elt | (make (- N 1) Elt)]))

(define init
  { (number --> A) --> (t A) }
  F -> (init-h 0 F))

(define init-h
  { number --> (number --> A) --> (t A) }
  N F -> (mfreeze [(F N) | (init-h (+ N 1) F)]))

(define range-step
  { number --> number --> number --> (t number) }
  Step Start End -> (range-step-increasing-h Step Start End) where (> Step 0)
  Step Start End -> (range-step-decreasing-h (- 0 Step) Start End) where (< Step 0)
  _ _ _ -> (error "seq.range-step called with Step=0"))

(define range-step-increasing-h
  { number --> number --> number --> (t number) }
  Step Start End -> (empty) where (> Start End)
  Step Start End -> (mfreeze [Start | (range-step-increasing-h Step (+ Start Step) End)]))

(define seq.range-step-decreasing-h
  { number --> number --> number --> (t number) }
  Step Start End -> (empty) where (< Start End)
  Step Start End -> (mfreeze [Start | (range-step-decreasing-h Step (- Start Step) End)]))

(define seq.range
  { number --> number --> (t number) }
  Start End -> (range-step 1 Start End) where (>= End Start)
  Start End -> (range-step -1 Start End))

\\ TODO: of-vector/string/dict

(define of-list
  { (list A) --> (t A) }
  [] -> (empty)
  [X | Xs] -> (seq.cons X (of-list Xs)))

\\ TODO: to-vector/string/dict

(define to-list
  { (t A) --> (list A) }
  S -> (to-list-h (thaw S)))

(define to-list-h
  { (node A) --> (list A) }
  [] -> []
  [X | Seq] -> [X | (to-list-h (thaw Seq))])

(define forever
  { (lazy A) --> (t A) }
  L -> (mfreeze [(thaw L) | (forever L)]))

\\ Predicates

(define seq.empty?
  { (t A) --> boolean}
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
  { (node A) --> (t A) }
  [] -> (error "seq.tail called on empty seq")
  [_ | T] -> T)

(define seq.head
  { (t A) --> A }
  S -> (node-head (thaw S)))

(define seq.tail
  { (t A) --> (t A) }
  S -> (node-tail (thaw S)))

\\ Consumption

(define fold-left
  { (A --> B --> A) --> A --> (t B) --> A }
  F Init S -> (fold-left-h F Init (thaw S)))

(define fold-left-h
  { (A --> B --> A) --> A --> (node B) --> A }
  _ Acc [] -> Acc
  F Acc [H | T] -> (fold-left-h F (F Acc H) (thaw T)))

(define for-each
  { (A --> Any) --> (t A) --> (list unit) }
  F S -> (for-each-h F (thaw S)))

(define for-each-h
  { (A --> Any) --> (node A) --> (list unit)}
  _ [] -> []
  F [H | T] -> (do (F H) (for-each-h F (thaw T))))

(define equal?
  { (t A) --> (t A) --> boolean }
  S1 S2 -> (equal?-h (thaw S1) (thaw S2)))

(define equal?-h
  { (node A) --> (node A) --> boolean }
  [] [] -> true
  [X | XSeq] [X | YSeq] -> (equal?-h (thaw XSeq) (thaw YSeq))
  _ _ -> false)

(define equal-cmp?
  { (A --> B --> boolean) --> (t A) --> (t B) --> boolean }
  Cmp S1 S2 -> (equal-cmp?-h Cmp (thaw S1) (thaw S2)))

(define equal-cmp?-h
  { (A --> B --> boolean) --> (node A) --> (node B) --> boolean }
  _ [] [] -> true
  Cmp [X | XSeq] [Y | YSeq] -> (equal-cmp?-h Cmp (thaw XSeq) (thaw YSeq)) where (Cmp X Y)
  _ _ _ -> false)

(define for-all?
  { (A --> boolean) --> (t A) --> boolean }
  F S -> (for-all?-h F (thaw S)))

(define for-all?-h
  { (A --> boolean) --> (node A) --> boolean }
  F [] -> true
  F [X | Seq] -> (for-all?-h F (thaw Seq)) where (F X)
  _ _ -> false)

(define exists?
  { (A --> boolean) --> (t A) --> boolean }
  F S -> (exists?-h F (thaw S)))

(define exists?-h
  { (A --> boolean) --> (node A) --> boolean }
  F [] -> false
  F [X | _] -> true where (F X)
  F [X | Seq] -> (exists?-h F (thaw Seq)))

(define seq.element?
  { A --> (t A) --> boolean }
  X S -> (exists? (= X) S))

(define element-cmp?
  { (A --> B --> boolean) --> A --> (t B) --> boolean }
  Cmp X S -> (exists? (Cmp X) S))

(define find
  { (A --> boolean) --> (t A) --> (maybe.t A) }
  F S -> (find-h F (thaw S)))

(define find-h
  { (A --> boolean) --> (node A) --> (maybe.t A) }
  _ [] -> (@none)
  F [X | Seq] -> (if (F X)
                     (@some X)
                     (find-h F (thaw Seq))))

(define find-map
  { (A --> (maybe.t B)) --> (t A) --> (maybe.t B) }
  F S -> (find-map-h F (thaw S)))

(define find-map-h
  { (A --> (maybe.t B)) --> (node A) --> (maybe.t B) }
  _ [] -> (@none)
  F [X | Seq] -> (let Result (F X)
                   (if (maybe.some? Result)
                       Result
                       (find-map-h F (thaw Seq)))))

\\ Transformation

(define seq.map
  { (A --> B) --> (t A) --> (t B) }
  F S -> (mfreeze (map-h F (thaw S))))

(define map-h
  { (A --> B) --> (node A) --> (node B)}
  _ [] -> []
  F [H | T] -> [(F H) | (mfreeze (map-h F (thaw T)))])

(define filter
  { (A --> boolean) --> (t A) --> (t A) }
  F S -> (mfreeze (filter-h F (thaw S))))

(define filter-h
  { (A --> boolean) --> (node A) --> (node A)}
  _ [] -> []
  F [H | T] -> [H | (mfreeze (filter-h F (thaw T)))]
      where (F H)
  F [H | T] -> (filter-h F (thaw T)))

(define filter-map
  { (A --> (maybe.t B)) --> (t A) --> (t B) }
  F S -> (filter-map-h F (thaw S)))

(define filter-map-h
  { (A --> (maybe.t B)) --> (node A) --> (t B) }
  F [V | Seq] -> (filter-map-hh F (F V) Seq))

(define filter-map-hh
  { (A --> (maybe.t B)) --> (maybe.t B) --> (t A) --> (t B) }
  F V Seq -> (mfreeze [(maybe.get V) | (filter-map F Seq)]) where (maybe.some? V)
  F _ Seq -> (filter-map F Seq))

(define flat-map
  { (A --> (t B)) --> (t A) --> (t B) }
  F A -> (flat-map-h F (thaw A)))

(define flat-map-h
  { (A --> (t B)) --> (node A) --> (t B) }
  _ [] -> (freeze [])
  F [X | Next] -> (flat-map-append F (F X) Next))

(define flat-map-append
  { (A --> (t B)) --> (t B) --> (t A) --> (t B) }
  F S Tail -> (flat-map-append-h F (thaw S) Tail))

(define flat-map-append-h
  { (A --> (t B)) --> (node B) --> (t A) --> (t B) }
  F [] Tail -> (flat-map F Tail)
  F [X | Next] Tail -> (mfreeze [X | (flat-map-append F Next Tail)]))

(define seq.append
  { (t A) --> (t A) --> (t A) }
  A B -> (append-h (thaw A) B))

(define append-h
  { (node A) --> (t A) --> (t A) }
  [] B -> B
  [H | T] B -> (mfreeze [H | (seq.append T B)]))

(define seq.concat
  { (list (t A)) --> (t A)}
  [] -> (empty)
  [S | Ss] -> (seq.append S (seq.concat Ss)))

(define flatten
  { (t (t A)) --> (t A)}
  S -> (flatten-h (thaw S)))

(define flatten-h
  { (node (t A)) --> (t A)}
  [] -> (empty)
  [S | Ss] -> (seq.append S (flatten-h (thaw Ss))))

(define cycle
  { (t A) --> (t A) }
  S -> (mfreeze (thaw (seq.append S (cycle S)))))

(define take
  { number --> (t A) --> (t A) }
  0 _ -> (empty)
  N S -> (mfreeze
          (thaw (seq.cons (seq.head S)
                          (take (- N 1) (seq.tail S))))))

(define drop
  { number --> (t A) --> (t A) }
  0 S -> S
  N S -> (mfreeze (thaw (drop (- N 1) (seq.tail S)))))

(define take-while
  { (A --> boolean) --> (t A) --> (t A) }
  F S -> (empty) where (not (F (seq.head S)))
  F S -> (mfreeze
          (thaw (seq.cons (seq.head S)
                          (take-while F (seq.tail S))))))

(define drop-while
  { (A --> boolean) --> (t A) --> (t A) }
  F S -> S where (not (F (seq.head S)))
  F S -> (mfreeze (thaw (drop-while F (seq.tail S)))))

(define zip-with
  { (A --> B --> C) --> (t A) --> (t B) --> (t C) }
  Cons S1 S2 -> (zip-with-h Cons (thaw S1) (thaw S2)))

(define zip-with-h
  { (A --> B --> C) --> (node A) --> (node B) --> (t C) }
  _ [] _ -> (empty)
  _ _ [] -> (empty)
  Cons [X | XSeq] [Y | YSeq] -> (mfreeze [(Cons X Y) | (zip-with-h Cons (thaw XSeq) (thaw YSeq))]))

(define zip
  { (t A) --> (t B) --> (t (A * B)) }
  S1 S2 -> (zip-h (thaw S1) (thaw S2)))

(define zip-h
  { (node A) --> (node B) --> (t (A * B))}
  [] _ -> (empty)
  _ [] -> (empty)
  [X | XSeq] [Y | YSeq] -> (mfreeze [(@p X Y) | (zip-h (thaw XSeq) (thaw YSeq))]))

(define unzip
  { (t (A * B)) --> ((t A) * (t B)) }
  S -> (@p (seq.map (function fst) S)
           (seq.map (function snd) S)))

\\ TODO: chunks { number -> (t A) -> (seq (vector A)) }

(preclude [seq-internal])

)
