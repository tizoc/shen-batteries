\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\** {1 Lazy sequences} *\

\** The type [(seq.t A)] represents a delayed sequence of values of type [A]. Evaluation
    of each element in the sequence is delayed until the element is accessed when the
    sequence is traversed. The results of sequence transformations are also delayed,
    and no evaluation will happen until the sequence produced by the transformation
    is traversed.
*\

(package seq [any.t maybe.t maybe.some? maybe.unsafe-get lazy.memo @none @some]

(datatype t-internal
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
  X : (mode (list A) -);

  (vector? X) : verified;
  ______________________
  X : (mode (vector A) -);
  )

(datatype t
  X : (list A);
  ______________________
  X : (mode (like A) -);

  X : (vector A);
  ______________________
  X : (mode (like A) -);
  )

\** {2 Creation} *\

\** [(seq.empty)] produces the empty sequence. *\
(define empty
  { --> (t A) }
  -> (freeze []))

\** [(seq.singleton X)] produces a singleton sequence containing just [X]. *\
(define singleton
  { A --> (t A) }
  X -> (freeze [X | (empty)]))

\** [(memo Seq)] wraps [Seq] in a memoized sequence for which elements are
    evaluated only once the first time the sequence is traversed. *\
(define memo
  { (t A) --> (t A) }
  Seq -> (lazy.memo (freeze (memo-h (thaw Seq)))))

(define memo-h
  { (node A) --> (node A) }
  [] -> []
  [X | Seq] -> [X | (memo Seq)])

\** [(seq.cons V S)] produces a new sequence with [V] as the first element, followed by
    everything contained in sequence [S]. *\
(define seq.cons
  { A --> (t A) --> (t A) }
  V S -> (freeze [V | S]))

\** [(seq.snoc S V)] produces a new sequence with everything in sequence [S], followed by [V]. *\
(define snoc
  { (t A) --> A --> (t A) }
  S V -> (seq.append S (singleton V)))

\** [(seq.make N Elt)] produces a sequence containing [N] times the value of [Elt]. *\
(define make
  { number --> A --> (t A) }
  0 _ -> (empty)
  N Elt -> (freeze [Elt | (make (- N 1) Elt)]))

\** [(seq.unfold F Seed)] produces a sequence of values constructed from the results of [(F Seed)].
    [F] must return [(@none)] to signal the end of the production of elements, and [(@some Elt NewSeed)]
    to produce a new sequence element [Elt]. [NewSeed] will be passed to [F] the next time a sequence
    element has to be produced. *\
(define unfold
  { (B --> (maybe.t (A * B))) --> B --> (t A) }
  F Seed -> (freeze
              (let MaybeResult (F Seed)
                 (if (maybe.some? MaybeResult)
                     (let Result (maybe.unsafe-get MaybeResult)
                          Elt (fst Result)
                          NewSeed (snd Result)
                       [Elt | (unfold F NewSeed)])
                     []))))

\** [(seq.range-step Step Start End)] produces a sequence containing all integers in the range (inclusive) from
    [Start] to [End] and separated by [Step]. If [Step] is a negative number, a sequence of decreasing values
    is produced. *\
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

\** [(seq.range Start End)] produces a sequence containing all integers in the range (inclusive) from
    [Start] to [End]. If [Start] is greater than [End], a sequence of decreasing values is produced. *\
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

\** [(seq.of SeqLike)] produces a sequence containing all elements in [SeqLike], where [SeqLike]
    is either a list, or a vector. *\
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

\** [(seq.of-list List)] produces a sequence containing all elements in the list [List]. *\
(define of-list
  { (list A) --> (t A) }
  [] -> (empty)
  [X | Xs] -> (freeze [X | (of-list Xs)]))

\** [(seq.of-vector Vector)] produces a sequence containing all elements in the vector [Vector]. *\
(define of-vector
  { (vector A) --> (t A) }
  V -> (of-vector-h V 1 (limit V)))

(define of-vector-h
  { (vector A) --> number --> number --> (t A) }
  _ N L -> (empty) where (> N L)
  V N L -> (freeze [(<-vector V N) | (of-vector-h V (+ N 1) L)]))

\** [(seq.of-vector Vector)] produces a sequence containing all elements in the vector [Vector] in reverse order. *\
(define of-vector-reversed
  { (vector A) --> (t A) }
  V -> (of-vector-reversed-h V (limit V)))

(define of-vector-reversed-h
  { (vector A) --> number --> (t A) }
  _ 0 -> (empty)
  V N -> (freeze [(<-vector V N) | (of-vector-reversed-h V (- N 1))]))

\** [(seq.of-string String)] produces a sequence of unit strings, each one being a character in the string [String]. *\
(define of-string
  { string --> (t string) }
  "" -> (empty)
  S -> (freeze (of-string-h S 0)))

(define of-string-h
  { string --> number --> (node string) }
  S N -> (let Char (trap-error (pos S N) (/. _ ""))
           (if (= Char "")
               []
               [Char | (freeze (of-string-h S (+ N 1)))])))

\\ TODO: dict

\** [(seq.to-list Seq)] constructs a list containing every element produced by the sequence [Seq]. *\
(define to-list
  { (t A) --> (list A) }
  S -> (to-list-h (thaw S)))

(define to-list-h
  { (node A) --> (list A) }
  [] -> []
  [X | Seq] -> [X | (to-list-h (thaw Seq))])

\** [(seq.into-vector Start Count Vector Seq)] fills the vector [Vector] with elements produced by the sequence [Seq].
    If [Count] is positive, the vector slots from [Start] to [Start + Count] (not included) are filled in increasing order.
    If [Count] is negative, the vector slots from [Start] to [Start - Count] (not included) are filled in decreasing order.
    The return value is a [(@p RemainingSeq NotFilledCount)] tuple, with [RemainingSeq] being what is left to be consumed of the sequence, and [NotFilledCount]
    the amount of slots that couldn't be filled because [Seq] got fully consumed before [Count] amount of slots were filled. If [NotFilledCount] is [0],
    that means that the operation succeeded without the [Seq] sequence ending prematurely. *\
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

\** [(seq.to-string Seq)] constructs a list that is the concatenation of every string produced by the sequence [Seq]. *\
(define to-string
  { (t string) --> string }
  S -> (to-string-h (thaw S)))

(define to-string-h
  { (node string) --> string }
  [] -> ""
  [S | Seq] -> (@s S (to-string-h (thaw Seq))))

\** [(seq.forever Frozen)] produces an infinite sequence that always produces values that are the result of [(thaw Frozen)]. *\
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

(define drain
  { (t A) --> unit }
  Seq -> (for-each (/. _ unit) Seq))

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

(define truncate
  { number --> (t A) --> (t A) }
  N _ -> (error "cannot truncate a negative amount from a seq") where (< N 0)
  N S -> (freeze (truncate-h N (thaw S))))

(define truncate-h
  { number --> (node A) --> (node A) }
  0 _ -> []
  _ [] -> []
  N [X | Seq] -> [X | (truncate (- N 1) Seq)])

(define take
  { number --> (t A) --> (t A) }
  N _ -> (error "cannot take a negative amount from a seq") where (< N 0)
  N S -> (freeze (take-h N (thaw S))))

(define take-h
  { number --> (node A) --> (node A) }
  0 _ -> []
  N [] -> (error "failure to take from sequence that ended abruptly")
  N [X | Seq] -> [X | (take (- N 1) Seq)])

(define drop
  { number --> (t A) --> (t A) }
  0 S -> S
  N _ -> (error "cannot drop a negative amount from a seq") where (< N 0)
  N S -> (freeze (thaw (drop-h N (thaw S)))))

(define drop-h
  { number --> (node A) --> (t A) }
  N [] -> (error "failure to drop from sequence that ended abruptly")
  1 [X | Seq] -> Seq
  N [X | Seq] -> (drop-h (- N 1) (thaw Seq)))

(define take-while
  { (A --> boolean) --> (t A) --> (t A) }
  F S -> (freeze (take-while-h F (thaw S))))

(define take-while-h
  { (A --> boolean) --> (node A) --> (node A) }
  _ [] -> []
  F [X | Seq] -> (if (F X)
                     [X | (take-while F Seq)]
                     []))

(define drop-while
  { (A --> boolean) --> (t A) --> (t A) }
  F S -> (freeze (drop-while-h F (thaw S))))

(define drop-while-h
  { (A --> boolean) --> (node A) --> (node A) }
  _ [] -> []
  F [X | Seq] -> (if (F X)
                     (drop-while-h F (thaw Seq))
                     [X | Seq]))

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
