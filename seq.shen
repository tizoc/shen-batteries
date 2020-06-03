\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package seq [any.t maybe.t maybe.some? maybe.unsafe-get lazy.memo @none @some]

(datatype internal-type
  ______________________
  [] : (mode (node A) -);

  X : A;
  Seq : (t A);
  ===================
  (cons X Seq) : (mode (node A) -);

  Node : (node A);
  ______________________
  (freeze Node) : (mode (t A) -);

  Seq : (t A);
  ______________________
  (lazy.memo Seq) : (mode (t A) -);

  Seq : (t A);
  ______________________
  (thaw Seq) : (node A);

  (list? X) : verified;
  ______________________
  X : (list A);

  (vector? X) : verified;
  ______________________
  X : (vector A);
  )

(datatype t
  X : (list A);
  ______________________
  X : (like A);

  X : (vector A);
  ______________________
  X : (like A);
  )

\\ Creation

(define empty
  { --> (t A) }
  -> (freeze []))

(define singleton
  { A --> (t A) }
  X -> (freeze [X | (empty)]))

(define memo
  { (t A) --> (t A) }
  Seq -> (lazy.memo (freeze (memo-h (thaw Seq)))))

(define memo-h
  { (node A) --> (node A) }
  [] -> []
  [X | Seq] -> [X | (memo Seq)])

(define seq.cons
  { A --> (t A) --> (t A) }
  V S -> (freeze [V | S]))

(define snoc
  { (t A) --> A --> (t A) }
  S V -> (seq.append S (singleton V)))

(define make
  { number --> A --> (t A) }
  0 _ -> (empty)
  N Elt -> (freeze [Elt | (make (- N 1) Elt)]))

(define init
  { (number --> A) --> (t A) }
  F -> (init-h 0 F))

(define init-h
  { number --> (number --> A) --> (t A) }
  N F -> (freeze [(F N) | (init-h (+ N 1) F)]))

(define range-step
  { number --> number --> number --> (t number) }
  Step Start End -> (range-step-increasing-h Step Start End) where (> Step 0)
  Step Start End -> (range-step-decreasing-h (- 0 Step) Start End) where (< Step 0)
  _ _ _ -> (error "seq.range-step called with Step=0"))

(define range-step-increasing-h
  { number --> number --> number --> (t number) }
  Step Start End -> (empty) where (> Start End)
  Step Start End -> (freeze [Start | (range-step-increasing-h Step (+ Start Step) End)]))

(define seq.range-step-decreasing-h
  { number --> number --> number --> (t number) }
  Step Start End -> (empty) where (< Start End)
  Step Start End -> (freeze [Start | (range-step-decreasing-h Step (- Start Step) End)]))

(define seq.range
  { number --> number --> (t number) }
  Start End -> (range-step 1 Start End) where (>= End Start)
  Start End -> (range-step -1 Start End))

\\ TODO: dict

(define list?
  { any.t --> boolean }
  [] -> true
  [_ | _] -> true
  _ -> false)

(define of
  { (like A) --> (t A) }
  L -> (of-list L) where (list? L)
  V -> (of-vector V) where (vector? V)
  _ -> (error "seq.of can only convert from lists and vectors"))

(define of-freeze
  { (lazy A) --> (t A) }
  L -> (freeze [(thaw L) | (empty)]))

(define of-freeze-seq
  { (lazy (t A)) --> (t A) }
  L -> (thaw L))

(define of-list
  { (list A) --> (t A) }
  [] -> (empty)
  [X | Xs] -> (freeze [X | (of-list Xs)]))

(define of-vector
  { (vector A) --> (t A) }
  V -> (of-vector-h V 1 (limit V)))

(define of-vector-h
  { (vector A) --> number --> number --> (t A) }
  _ N L -> (empty) where (> N L)
  V N L -> (freeze [(<-vector V N) | (of-vector-h V (+ N 1) L)]))

(define of-vector-reversed
  { (vector A) --> (t A) }
  V -> (of-vector-reversed-h V (limit V)))

(define of-vector-reversed-h
  { (vector A) --> number --> (t A) }
  _ 0 -> (empty)
  V N -> (freeze [(<-vector V N) | (of-vector-reversed-h V (- N 1))]))

\\ TODO: faster version without @s
(define of-string
  { string --> (t string) }
  "" -> (empty)
  (@s S Ss) -> (freeze [S | (of-string Ss)]))

\\ TODO: dict

(define to-list
  { (t A) --> (list A) }
  S -> (to-list-h (thaw S)))

(define to-list-h
  { (node A) --> (list A) }
  [] -> []
  [X | Seq] -> [X | (to-list-h (thaw Seq))])

(define into-vector
  { number --> number --> (vector A) --> (t A) --> ((t A) * number) }
  Start Count Vec _ -> (error "start position out of vector range")
      where (or (< Start 1)
                (> Start (limit Vec)))
  Start Count Vec _ -> (error "count exceeds vector limits")
      where (or (< (+ Start Count) 0)
                (> (+ Start Count) (+ 1 (limit Vec))))
  _ 0 _ Seq -> (@p Seq 0)
  Start Count Vec Seq -> (into-vector-h Start (- 0 Count) -1 Vec (thaw Seq)) where (< Count 0)
  Start Count Vec Seq -> (into-vector-h Start Count 1 Vec (thaw Seq)))

(define into-vector-h
  { number --> number --> number --> (vector A) --> (node A) --> ((t A) * number) }
  N Count _ _ [] -> (@p (empty) Count)
  N 1 _ V [X | Seq] -> (do (vector-> V N X)
                           (@p Seq 0))
  N Count Step V [X | Seq] -> (into-vector-h (+ N Step) (- Count 1) Step (vector-> V N X) (thaw Seq)))

(define to-string
  { (t string) --> string }
  S -> (to-string-h (thaw S)))

(define to-string-h
  { (node string) --> string }
  [] -> ""
  [S | Seq] -> (@s S (to-string-h (thaw Seq))))

(define forever
  { (lazy A) --> (t A) }
  L -> (freeze [(thaw L) | (forever L)]))

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
  { (A --> Any) --> (t A) --> unit }
  F S -> (for-each-h F (thaw S)))

(define for-each-h
  { (A --> Any) --> (node A) --> unit }
  _ [] -> unit
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
  F S -> (freeze (map-h F (thaw S))))

(define map-h
  { (A --> B) --> (node A) --> (node B)}
  _ [] -> []
  F [H | T] -> [(F H) | (freeze (map-h F (thaw T)))])

(define filter
  { (A --> boolean) --> (t A) --> (t A) }
  F S -> (freeze (filter-h F (thaw S))))

(define filter-h
  { (A --> boolean) --> (node A) --> (node A)}
  _ [] -> []
  F [H | T] -> [H | (freeze (filter-h F (thaw T)))]
      where (F H)
  F [H | T] -> (filter-h F (thaw T)))

(define filter-map
  { (A --> (maybe.t B)) --> (t A) --> (t B) }
  F S -> (freeze (filter-map-h F (thaw S))))

(define filter-map-h
  { (A --> (maybe.t B)) --> (node A) --> (node B) }
  _ [] -> []
  F [V | Seq] -> (filter-map-hh F (F V) Seq))

(define filter-map-hh
  { (A --> (maybe.t B)) --> (maybe.t B) --> (t A) --> (node B) }
  F V Seq -> [(maybe.unsafe-get V) | (filter-map F Seq)] where (maybe.some? V)
  F _ Seq -> (filter-map-h F (thaw Seq)))

(define flat-map
  { (A --> (t B)) --> (t A) --> (t B) }
  F A -> (freeze (flat-map-h F (thaw A))))

(define flat-map-h
  { (A --> (t B)) --> (node A) --> (node B) }
  _ [] -> []
  F [X | Next] -> (flat-map-append F (F X) Next))

(define flat-map-append
  { (A --> (t B)) --> (t B) --> (t A) --> (node B) }
  F S Tail -> (flat-map-append-h F (thaw S) Tail))

(define flat-map-append-h
  { (A --> (t B)) --> (node B) --> (t A) --> (node B) }
  F [] Tail -> (flat-map-h F (thaw Tail))
  F [X | Next] Tail -> [X | (freeze (flat-map-append F Next Tail))])

(define seq.append
  { (t A) --> (t A) --> (t A) }
  A B -> (freeze (append-h (thaw A) B)))

(define append-h
  { (node A) --> (t A) --> (node A) }
  [] B -> (thaw B)
  [H | T] B -> [H | (seq.append T B)])

(define seq.concat
  { (list (t A)) --> (t A)}
  [] -> (empty)
  [S | Ss] -> (seq.append S (seq.concat Ss)))

(define flatten
  { (t (t A)) --> (t A) }
  S -> (freeze (thaw (flatten-h (thaw S)))))

(define flatten-h
  { (node (t A)) --> (t A) }
  [] -> (empty)
  [S | Ss] -> (seq.append S (flatten-h (thaw Ss))))

(define cycle
  { (t A) --> (t A) }
  S -> (freeze (thaw (seq.append S (cycle S)))))

\\ FIXME: these will evaluate the head twice
(define take
  { number --> (t A) --> (t A) }
  0 _ -> (empty)
  N S -> (freeze
          (thaw (seq.cons (seq.head S)
                          (take (- N 1) (seq.tail S))))))

(define drop
  { number --> (t A) --> (t A) }
  0 S -> S
  N S -> (freeze (thaw (drop (- N 1) (seq.tail S)))))

(define take-while
  { (A --> boolean) --> (t A) --> (t A) }
  F S -> (empty) where (not (F (seq.head S)))
  F S -> (freeze
          (thaw (seq.cons (seq.head S)
                          (take-while F (seq.tail S))))))

(define drop-while
  { (A --> boolean) --> (t A) --> (t A) }
  F S -> S where (not (F (seq.head S)))
  F S -> (freeze (thaw (drop-while F (seq.tail S)))))

(define zip-with
  { (A --> B --> C) --> (t A) --> (t B) --> (t C) }
  Cons S1 S2 -> (freeze (zip-with-h Cons (thaw S1) (thaw S2))))

(define zip-with-h
  { (A --> B --> C) --> (node A) --> (node B) --> (node C) }
  _ [] _ -> []
  _ _ [] -> []
  Cons [X | XSeq] [Y | YSeq] -> [(Cons X Y) | (zip-with Cons XSeq YSeq)])

(define zip
  { (t A) --> (t B) --> (t (A * B)) }
  S1 S2 -> (freeze (zip-h (thaw S1) (thaw S2))))

(define zip-h
  { (node A) --> (node B) --> (node (A * B))}
  [] _ -> []
  _ [] -> []
  [X | XSeq] [Y | YSeq] -> [(@p X Y) | (zip XSeq YSeq)])

(define unzip
  { (t (A * B)) --> ((t A) * (t B)) }
  S -> (@p (seq.map (function fst) S)
           (seq.map (function snd) S)))

(define chunks
  { number --> (t A) --> (t (vector A)) }
  N _ -> (error "cannot produce seq chunks of size < 1") where (< N 1)
  N Seq -> (freeze
             (let Chunk (vector N)
               (chunks-h N (into-vector 1 N Chunk Seq) Chunk))))

(define chunks-h
  { number --> ((t A) * number) --> (vector A) --> (node (vector A)) }
  N (@p Seq 0) Chunk -> [Chunk | (chunks N Seq)]
  N (@p Seq N) _ -> []
  N (@p Seq Remaining) Chunk -> [(adjust-chunk Chunk (- N Remaining)) | (empty)])

(define adjust-chunk
  { (vector A) --> number --> (vector A) }
  V N -> (let NewChunk (vector N)
              _ (into-vector 1 N NewChunk (of-vector V))
            NewChunk))

(preclude [seq-internal])

)
