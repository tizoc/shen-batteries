\\ Copyright (c) 2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Lazy sequences
\\:
\\: The type `(seq.t A)` represents a delayed sequence of values of type `A`. Evaluation
\\: of each element in the sequence is delayed until the element is accessed when the
\\: sequence is traversed. The results of sequence transformations are also delayed,
\\: and no evaluation will happen until the sequence produced by the transformation
\\: is traversed.

(package seq [void maybe.t maybe.some? maybe.unsafe-get lazy.memo @none @some]

(datatype t-internal
  ______________________
  [] : (mode (node A) -);

  X : A;
  Seq : (seq.t A);
  ===================
  (cons X Seq) : (mode (node A) -);

  Node : (node A);
  ______________________
  (freeze Node) : (mode (seq.t A) -);

  Seq : (seq.t A);
  ______________________
  (lazy.memo Seq) : (mode (seq.t A) -);

  Seq : (seq.t A);
  ______________________
  (thaw Seq) : (node A);

  ______________________
  (list? X) : verified >> X : (mode (list A) -);

  ______________________
  (vector? X) : verified >> X : (mode (vector A) -);
  )

\\: == API

\\: === Creation

\\: `(seq.empty)` produces the empty sequence.
(define empty
  { --> (seq.t A) }
  -> (freeze []))

\\: `(seq.singleton X)` produces a singleton sequence containing just `X`.
(define singleton
  { A --> (seq.t A) }
  X -> (freeze [X | (empty)]))

\\: `(seq.memo Seq)` wraps `Seq` in a memoized sequence for which elements are
\\: evaluated only once the first time the sequence is traversed.
(define memo
  { (seq.t A) --> (seq.t A) }
  Seq -> (lazy.memo (freeze (memo-h (thaw Seq)))))

(define memo-h
  { (node A) --> (node A) }
  [] -> []
  [X | Seq] -> [X | (memo Seq)])

\\: `(seq.cons V S)` produces a new sequence with `V` as the first element, followed by
\\: everything contained in sequence `S`.
(define seq.cons
  { A --> (seq.t A) --> (seq.t A) }
  V S -> (freeze [V | S]))

\\: `(seq.snoc S V)` produces a new sequence with everything in sequence `S`, followed by `V`.
(define snoc
  { (seq.t A) --> A --> (seq.t A) }
  S V -> (seq.append S (singleton V)))

\\: `(seq.make N Elt)` produces a sequence containing `N` times the value of `Elt`.
(define make
  { number --> A --> (seq.t A) }
  0 _ -> (empty)
  N Elt -> (freeze [Elt | (make (- N 1) Elt)]))

\\: `(seq.unfold F Seed)` produces a sequence of values constructed from the results of `(F Seed)`.
\\: `F` must return `(@none)` to signal the end of the production of elements, and `(@some Elt NewSeed)`
\\: to produce a new sequence element `Elt`. `NewSeed` will be passed to `F` the next time a sequence
\\: element has to be produced.
(define unfold
  { (B --> (maybe.t (A * B))) --> B --> (seq.t A) }
  F Seed -> (freeze
              (let MaybeResult (F Seed)
                 (if (maybe.some? MaybeResult)
                     (let Result (maybe.unsafe-get MaybeResult)
                          Elt (fst Result)
                          NewSeed (snd Result)
                       [Elt | (unfold F NewSeed)])
                     []))))

\\: `(seq.range-step Step Start End)` produces a sequence containing all integers in the range (inclusive) from
\\: `Start` to `End` and separated by `Step`. If `Step` is a negative number, a sequence of decreasing values
\\: is produced.
(define range-step
  { number --> number --> number --> (seq.t number) }
  Step Start End -> (range-step-increasing-h Step Start End) where (> Step 0)
  Step Start End -> (range-step-decreasing-h (- 0 Step) Start End) where (< Step 0)
  _ _ _ -> (error "seq.range-step called with Step=0"))

(define range-step-increasing-h
  { number --> number --> number --> (seq.t number) }
  Step Start End -> (empty) where (> Start End)
  Step Start End -> (freeze [Start | (range-step-increasing-h Step (+ Start Step) End)]))

(define seq.range-step-decreasing-h
  { number --> number --> number --> (seq.t number) }
  Step Start End -> (empty) where (< Start End)
  Step Start End -> (freeze [Start | (range-step-decreasing-h Step (- Start Step) End)]))

\\: `(seq.range Start End)` produces a sequence containing all integers in the range (inclusive) from
\\: `Start` to `End`. If `Start` is greater than `End`, a sequence of decreasing values is produced.
(define seq.range
  { number --> number --> (seq.t number) }
  Start End -> (range-step 1 Start End) where (>= End Start)
  Start End -> (range-step -1 Start End))

\\ TODO: dict

(define list?
  { (or A (list A)) --> boolean }
  [] -> true
  [_ | _] -> true
  _ -> false)

\\: `(seq.of SeqLike)` produces a sequence containing all elements in `SeqLike`, where `SeqLike`
\\: is either a list, or a vector.
(define of
  { (or (list A) (vector A)) --> (seq.t A) }
  L -> (of-list L) where (list? L)
  V -> (of-vector V) where (vector? V)
  _ -> (error "seq.of can only convert from lists and vectors"))

(define of-freeze
  { (lazy A) --> (seq.t A) }
  L -> (freeze [(thaw L) | (empty)]))

(define of-freeze-seq
  { (lazy (seq.t A)) --> (seq.t A) }
  L -> (thaw L))

\\: `(seq.of-list List)` produces a sequence containing all elements in the list `List`.
(define of-list
  { (list A) --> (seq.t A) }
  [] -> (empty)
  [X | Xs] -> (freeze [X | (of-list Xs)]))

\\: `(seq.of-vector Vector)` produces a sequence containing all elements in the vector `Vector`.
(define of-vector
  { (vector A) --> (seq.t A) }
  V -> (of-vector-h V 1 (limit V)))

(define of-vector-h
  { (vector A) --> number --> number --> (seq.t A) }
  _ N L -> (empty) where (> N L)
  V N L -> (freeze [(<-vector V N) | (of-vector-h V (+ N 1) L)]))

\\: `(seq.of-vector Vector)` produces a sequence containing all elements in the vector `Vector` in reverse order.
(define of-vector-reversed
  { (vector A) --> (seq.t A) }
  V -> (of-vector-reversed-h V (limit V)))

(define of-vector-reversed-h
  { (vector A) --> number --> (seq.t A) }
  _ 0 -> (empty)
  V N -> (freeze [(<-vector V N) | (of-vector-reversed-h V (- N 1))]))

\\: `(seq.of-string String)` produces a sequence of unit strings, each one being a character in the string `String`.
(define of-string
  { string --> (seq.t string) }
  "" -> (empty)
  S -> (freeze (of-string-h S 0)))

(define of-string-h
  { string --> number --> (node string) }
  S N -> (let Char (trap-error (pos S N) (/. _ ""))
           (if (= Char "")
               []
               [Char | (freeze (of-string-h S (+ N 1)))])))

\\ TODO: dict

\\: `(seq.to-list Seq)` constructs a list containing every element produced by the sequence `Seq`.
(define to-list
  { (seq.t A) --> (list A) }
  S -> (to-list-h (thaw S)))

(define to-list-h
  { (node A) --> (list A) }
  [] -> []
  [X | Seq] -> [X | (to-list-h (thaw Seq))])

\\: `(seq.into-vector Start Count Vector Seq)` fills the vector `Vector` with elements produced by the sequence `Seq`.
\\: If `Count` is positive, the vector slots from `Start` to `Start + Count - 1` are filled in increasing order.
\\: If `Count` is negative, the vector slots from `Start` to `Start - abs(Count) + 1` are filled in decreasing order.
\\: The return value is a `(@p RemainingSeq NotFilledCount)` tuple, with `RemainingSeq` being what is left to be consumed of the sequence, and `NotFilledCount`
\\: the amount of slots that couldn't be filled because `Seq` got fully consumed before `Count` amount of slots were filled. If `NotFilledCount` is `0`,
\\: that means that the operation succeeded without the `Seq` sequence ending prematurely.
(define into-vector
  { number --> number --> (vector A) --> (seq.t A) --> ((seq.t A) * number) }
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
  { number --> number --> number --> (vector A) --> (node A) --> ((seq.t A) * number) }
  N Count _ _ [] -> (@p (empty) Count)
  N 1 _ V [X | Seq] -> (do (vector-> V N X)
                           (@p Seq 0))
  N Count Step V [X | Seq] -> (into-vector-h (+ N Step) (- Count 1) Step (vector-> V N X) (thaw Seq)))

\\: `(seq.to-string Seq)` constructs a list that is the concatenation of every string produced by the sequence `Seq`.
(define to-string
  { (seq.t string) --> string }
  S -> (to-string-h (thaw S)))

(define to-string-h
  { (node string) --> string }
  [] -> ""
  [S | Seq] -> (@s S (to-string-h (thaw Seq))))

\\: `(seq.forever Frozen)` produces an infinite sequence that always produces values that are the result of `(thaw Frozen)`.
(define forever
  { (lazy A) --> (seq.t A) }
  L -> (freeze [(thaw L) | (forever L)]))

\\: === Predicates

\\: `(seq.empty? Seq)` will return `true` if `Seq` is an empty sequence, `false` otherwise. Note that
\\: performing this check will cause the evaluation of the first element of `Seq` if it is not empty.
(define seq.empty?
  { (seq.t A) --> boolean}
  S -> (empty-node? (thaw S)))

(define empty-node?
  { (node A) --> boolean}
  [] -> true
  _ -> false)

\\: === Accessors

(define node-head
  { (node A) --> A }
  [] -> (error "seq.head called on empty seq")
  [H | _] -> H)

(define node-tail
  { (node A) --> (seq.t A) }
  [] -> (error "seq.tail called on empty seq")
  [_ | T] -> T)

\\: `(seq.head Seq)` evaluates and returns the first element of `Seq`.
(define seq.head
  { (seq.t A) --> A }
  S -> (node-head (thaw S)))

\\: `(seq.tail Seq)` returns `Seq` without the first element. Note that this
\\: will cause the evaulation of the first element of `Seq`.
(define seq.tail
  { (seq.t A) --> (seq.t A) }
  S -> (node-tail (thaw S)))

\\: === Consumption

\\: `(seq.drain Seq)` consumes `Seq` until no more elements are left. The produced
\\: elements will be discarded.
(define drain
  { (seq.t A) --> void }
  Seq -> (for-each (/. _ (void)) Seq))

\\: `(seq.fold-left F Acc Seq)` consumes the sequence `Seq`, combining each of it's elements with
\\: the accumulator `Acc` by calling `(F Acc Elt)`. The result of each call to `F` is a new `NewAcc`
\\: value that will be passed as argument to the next call to `F` along with the next element in the
\\: `Seq` sequence: `(F NewAcc NextElt)`. The return value of `seq.fold-left` is the result of the
\\: final call to `F` when the sequence is fully consumed.
(define fold-left
  { (A --> B --> A) --> A --> (seq.t B) --> A }
  F Init S -> (fold-left-h F Init (thaw S)))

(define fold-left-h
  { (A --> B --> A) --> A --> (node B) --> A }
  _ Acc [] -> Acc
  F Acc [H | T] -> (fold-left-h F (F Acc H) (thaw T)))

\\: `(seq.for-each F Seq)` consumes `Seq` until no more elements are left. Each value
\\: produced is passed to `F` and the result discarded.
(define for-each
  { (A --> Any) --> (seq.t A) --> void }
  F S -> (for-each-h F (thaw S)))

(define for-each-h
  { (A --> Any) --> (node A) --> void }
  _ [] -> (void)
  F [H | T] -> (do (F H) (for-each-h F (thaw T))))

\\: `(seq.equal? SeqA SeqB)` consumes `SeqA` and `SeqB` one element at a time comparing
\\: the elements with `(= EltA EltB)` until `false` is returned or one of the sequences is fully consumed.
\\: The result is `true` if `false` is never returned and both sequences produce the same amount of values.
(define equal?
  { (seq.t A) --> (seq.t A) --> boolean }
  S1 S2 -> (equal?-h (thaw S1) (thaw S2)))

(define equal?-h
  { (node A) --> (node A) --> boolean }
  [] [] -> true
  [X | XSeq] [X | YSeq] -> (equal?-h (thaw XSeq) (thaw YSeq))
  _ _ -> false)

\\: `(seq.equal-cmp? Cmp SeqA SeqB)` consumes `SeqA` and `SeqB` one element at a time comparing
\\: the elements with `(Cmp EltA EltB)` until `false` is returned or one of the sequences is fully consumed.
\\: The result is `true` if `false` is never returned and both sequences produce the same amount of values.
(define equal-cmp?
  { (A --> B --> boolean) --> (seq.t A) --> (seq.t B) --> boolean }
  Cmp S1 S2 -> (equal-cmp?-h Cmp (thaw S1) (thaw S2)))

(define equal-cmp?-h
  { (A --> B --> boolean) --> (node A) --> (node B) --> boolean }
  _ [] [] -> true
  Cmp [X | XSeq] [Y | YSeq] -> (equal-cmp?-h Cmp (thaw XSeq) (thaw YSeq)) where (Cmp X Y)
  _ _ _ -> false)

\\: `(seq.for-all? Test Seq)` return `true` if `(Test Elt)` is `true` for every value produced
\\: the traversal of `Seq`.
(define for-all?
  { (A --> boolean) --> (seq.t A) --> boolean }
  F S -> (for-all?-h F (thaw S)))

(define for-all?-h
  { (A --> boolean) --> (node A) --> boolean }
  F [] -> true
  F [X | Seq] -> (for-all?-h F (thaw Seq)) where (F X)
  _ _ -> false)

\\: `(seq.exists? Test Seq)` return `true` if `(Test Elt)` is `true` for any value produced
\\: by the traversal of `Seq`. `Seq` is only consumed until `(Test Elt)` is `true`.
(define exists?
  { (A --> boolean) --> (seq.t A) --> boolean }
  F S -> (exists?-h F (thaw S)))

(define exists?-h
  { (A --> boolean) --> (node A) --> boolean }
  F [] -> false
  F [X | _] -> true where (F X)
  F [X | Seq] -> (exists?-h F (thaw Seq)))

\\: `(seq.element? X Seq)` is equivalent to `(seq.exists? (= X) Seq)`.
(define seq.element?
  { A --> (seq.t A) --> boolean }
  X S -> (exists? (= X) S))

\\: `(seq.element? Cmp X Seq)` is equivalent to `(seq.exists? (Cmp X) Seq)`.
(define element-cmp?
  { (A --> B --> boolean) --> A --> (seq.t B) --> boolean }
  Cmp X S -> (exists? (Cmp X) S))

\\: `(seq.find Test Seq)` returns the first element in `Seq` for which `(Test Elt)` is `true`
\\: wrapped as `(@some Elt)` or `@none` otherwise.
(define find
  { (A --> boolean) --> (seq.t A) --> (maybe.t A) }
  F S -> (find-h F (thaw S)))

(define find-h
  { (A --> boolean) --> (node A) --> (maybe.t A) }
  _ [] -> (@none)
  F [X | Seq] -> (if (F X)
                     (@some X)
                     (find-h F (thaw Seq))))

\\: `(seq.find-map F Seq)` returns the first result of `(F Elt)` of the form `(@some Result)` or
\\: `(@none)` if there is no such result.
(define find-map
  { (A --> (maybe.t B)) --> (seq.t A) --> (maybe.t B) }
  F S -> (find-map-h F (thaw S)))

(define find-map-h
  { (A --> (maybe.t B)) --> (node A) --> (maybe.t B) }
  _ [] -> (@none)
  F [X | Seq] -> (let Result (F X)
                   (if (maybe.some? Result)
                       Result
                       (find-map-h F (thaw Seq)))))

\\: === Transformation

\\: `(seq.map F Seq)` returns a new sequence with all the elements in `Seq` transformed
\\: with `(F Elt)`.
(define seq.map
  { (A --> B) --> (seq.t A) --> (seq.t B) }
  F S -> (freeze (map-h F (thaw S))))

(define map-h
  { (A --> B) --> (node A) --> (node B)}
  _ [] -> []
  F [H | T] -> [(F H) | (freeze (map-h F (thaw T)))])

\\: `(seq.filter Test Seq)` returns a new sequence with all the elements in `Seq` for which
\\: `(Test Elt)` is `false` removed.
(define filter
  { (A --> boolean) --> (seq.t A) --> (seq.t A) }
  F S -> (freeze (filter-h F (thaw S))))

(define filter-h
  { (A --> boolean) --> (node A) --> (node A)}
  _ [] -> []
  F [H | T] -> [H | (freeze (filter-h F (thaw T)))]
      where (F H)
  F [H | T] -> (filter-h F (thaw T)))

\\: `(seq.filter F Seq)` returns a new sequence with all the elements in `Seq` for which
\\: `(F Elt)` is `(@none)` removed, and for which the result is `(@some NewElt)` replaced by
\\: `NewElt`.
(define filter-map
  { (A --> (maybe.t B)) --> (seq.t A) --> (seq.t B) }
  F S -> (freeze (filter-map-h F (thaw S))))

(define filter-map-h
  { (A --> (maybe.t B)) --> (node A) --> (node B) }
  _ [] -> []
  F [V | Seq] -> (filter-map-hh F (F V) Seq))

(define filter-map-hh
  { (A --> (maybe.t B)) --> (maybe.t B) --> (seq.t A) --> (node B) }
  F V Seq -> [(maybe.unsafe-get V) | (filter-map F Seq)] where (maybe.some? V)
  F _ Seq -> (filter-map-h F (thaw Seq)))

\\: `(seq.flat-map F Seq)` returns a sequence that is the concatenation of all subsequences
\\: produced by calling `(F Elt)` for each element in `Seq`.
(define flat-map
  { (A --> (seq.t B)) --> (seq.t A) --> (seq.t B) }
  F A -> (freeze (flat-map-h F (thaw A))))

(define flat-map-h
  { (A --> (seq.t B)) --> (node A) --> (node B) }
  _ [] -> []
  F [X | Next] -> (flat-map-append F (F X) Next))

(define flat-map-append
  { (A --> (seq.t B)) --> (seq.t B) --> (seq.t A) --> (node B) }
  F S Tail -> (flat-map-append-h F (thaw S) Tail))

(define flat-map-append-h
  { (A --> (seq.t B)) --> (node B) --> (seq.t A) --> (node B) }
  F [] Tail -> (flat-map-h F (thaw Tail))
  F [X | Next] Tail -> [X | (freeze (flat-map-append F Next Tail))])

\\: `(seq.append SeqA SeqB)` returns a sequence that produces all elements in `SeqA`
\\: followed by all elements in `SeqB`.
(define seq.append
  { (seq.t A) --> (seq.t A) --> (seq.t A) }
  A B -> (freeze (append-h (thaw A) B)))

(define append-h
  { (node A) --> (seq.t A) --> (node A) }
  [] B -> (thaw B)
  [H | T] B -> [H | (seq.append T B)])

\\: `(seq.concat [Seq1 Seq2 ... SeqN])` returns a sequence that produces all elements
\\: in `Seq1` followed by all elements in `Seq2`, .... followed by all elements in `SeqN`.
(define seq.concat
  { (list (seq.t A)) --> (seq.t A)}
  [] -> (empty)
  [S | Ss] -> (seq.append S (seq.concat Ss)))

\\: `(seq.flatten SeqOfSeqs)` returns a sequence that produces every element produced
\\: by each subsequence produced by `SeqOfSeqs`.
(define flatten
  { (seq.t (seq.t A)) --> (seq.t A) }
  S -> (freeze (thaw (flatten-h (thaw S)))))

(define flatten-h
  { (node (seq.t A)) --> (seq.t A) }
  [] -> (empty)
  [S | Ss] -> (seq.append S (flatten-h (thaw Ss))))

\\: `(seq.cycle Seq)` returns an infinite sequence that produces all the elements from `Seq`
\\: repeated from the beginning each time the end of the original sequence is reached.
(define cycle
  { (seq.t A) --> (seq.t A) }
  S -> (freeze (thaw (seq.append S (cycle S)))))

\\: `(seq.truncate N Seq)` returns a sequence containing at most the first `N` elements of `Seq`.
(define truncate
  { number --> (seq.t A) --> (seq.t A) }
  N _ -> (error "cannot truncate a negative amount from a seq") where (< N 0)
  N S -> (freeze (truncate-h N (thaw S))))

(define truncate-h
  { number --> (node A) --> (node A) }
  0 _ -> []
  _ [] -> []
  N [X | Seq] -> [X | (truncate (- N 1) Seq)])

\\: `(seq.take N Seq)` returns a sequence containing the first `N` elements of `Seq`.
\\: An error will be thrown when traversing the new sequence if the original sequence
\\: had fewer than `N` elements.
(define take
  { number --> (seq.t A) --> (seq.t A) }
  N _ -> (error "cannot take a negative amount from a seq") where (< N 0)
  N S -> (freeze (take-h N (thaw S))))

(define take-h
  { number --> (node A) --> (node A) }
  0 _ -> []
  N [] -> (error "failure to take from sequence that ended abruptly")
  N [X | Seq] -> [X | (take (- N 1) Seq)])

\\: `(seq.drop N Seq)` returns a sequence with the elements from `Seq` with the first `N` elements removed.
\\: An error will be thrown when traversing the new sequence if the original sequence
\\: had fewer than `N` elements.
(define drop
  { number --> (seq.t A) --> (seq.t A) }
  0 S -> S
  N _ -> (error "cannot drop a negative amount from a seq") where (< N 0)
  N S -> (freeze (thaw (drop-h N (thaw S)))))

(define drop-h
  { number --> (node A) --> (seq.t A) }
  N [] -> (error "failure to drop from sequence that ended abruptly")
  1 [X | Seq] -> Seq
  N [X | Seq] -> (drop-h (- N 1) (thaw Seq)))

\\: `(seq.take-while Test Seq)` returns a sequence containing elements of `Seq` for
\\: which `(Test Elt)` is `true` until `false` is returned or the sequence ends.
(define take-while
  { (A --> boolean) --> (seq.t A) --> (seq.t A) }
  F S -> (freeze (take-while-h F (thaw S))))

(define take-while-h
  { (A --> boolean) --> (node A) --> (node A) }
  _ [] -> []
  F [X | Seq] -> (if (F X)
                     [X | (take-while F Seq)]
                     []))

\\: `(seq.drop-while Test Seq)` returns a sequence containing elements of `Seq`
\\: skipping all elements for which `(Test Elt)` is `true` until `false` is returned
\\: or the sequence ends.
(define drop-while
  { (A --> boolean) --> (seq.t A) --> (seq.t A) }
  F S -> (freeze (drop-while-h F (thaw S))))

(define drop-while-h
  { (A --> boolean) --> (node A) --> (node A) }
  _ [] -> []
  F [X | Seq] -> (if (F X)
                     (drop-while-h F (thaw Seq))
                     [X | Seq]))

\\: `(seq.zip-with Cons SeqA SeqB)` returns a new sequence containing elements
\\: that are the result of calling `(Cons EltA EltB)` for each element produced
\\: by the parallel traversal of `SeqA` and `SeqB`. The produced sequence is
\\: as long as the shortest of the input sequences.
(define zip-with
  { (A --> B --> C) --> (seq.t A) --> (seq.t B) --> (seq.t C) }
  Cons S1 S2 -> (freeze (zip-with-h Cons (thaw S1) (thaw S2))))

(define zip-with-h
  { (A --> B --> C) --> (node A) --> (node B) --> (node C) }
  _ [] _ -> []
  _ _ [] -> []
  Cons [X | XSeq] [Y | YSeq] -> [(Cons X Y) | (zip-with Cons XSeq YSeq)])

\\: `(seq.zip SeqA SeqB)` is equivalent to `(seq.zip-with (/. A B (@p A B)) SeqA SeqB)`.
(define zip
  { (seq.t A) --> (seq.t B) --> (seq.t (A * B)) }
  S1 S2 -> (freeze (zip-h (thaw S1) (thaw S2))))

(define zip-h
  { (node A) --> (node B) --> (node (A * B))}
  [] _ -> []
  _ [] -> []
  [X | XSeq] [Y | YSeq] -> [(@p X Y) | (zip XSeq YSeq)])

\\: `(seq.unzip SeqA*B)` returns `(@p SeqA SeqB)`, where `SeqA*B` is a sequence
\\: of tuples `(@p A B)`, `SeqA` is `(seq.map (function fst) SeqA*B)` and `SeqB` is
\\: `(seq.map (function snd) SeqA*B)`.
(define unzip
  { (seq.t (A * B)) --> ((seq.t A) * (seq.t B)) }
  S -> (@p (seq.map (function fst) S)
           (seq.map (function snd) S)))

\\: `(seq.chunks N Seq)` returns a sequence of vectors of size `N`, with
\\: each vector filled with the elements obtained from taking `N` elements
\\: from `Seq`. The last vector may have a size smaller than `N` if the
\\: sequence ends before it can be fully filled.
(define chunks
  { number --> (seq.t A) --> (seq.t (vector A)) }
  N _ -> (error "cannot produce seq chunks of size < 1") where (< N 1)
  N Seq -> (freeze
             (let Chunk (vector N)
               (chunks-h N (into-vector 1 N Chunk Seq) Chunk))))

(define chunks-h
  { number --> ((seq.t A) * number) --> (vector A) --> (node (vector A)) }
  N (@p Seq 0) Chunk -> [Chunk | (chunks N Seq)]
  N (@p Seq N) _ -> []
  N (@p Seq Remaining) Chunk -> [(adjust-chunk Chunk (- N Remaining)) | (empty)])

(define adjust-chunk
  { (vector A) --> number --> (vector A) }
  V N -> (let NewChunk (vector N)
              _ (into-vector 1 N NewChunk (of-vector V))
            NewChunk))

(preclude [t-internal])

)
