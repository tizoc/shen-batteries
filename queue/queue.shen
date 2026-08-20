\\ Copyright (c) 2026 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

\\: = Persistent queues
\\:
\\: `(queue.t A)` is an immutable first-in, first-out queue. Operations return
\\: new queues and leave their arguments usable. The implementation uses a
\\: front list and a reversed rear list. `queue.snoc` takes constant time.
\\: `queue.uncons` also takes constant time unless the front is exhausted;
\\: then it reverses the rear list in time linear in that list's length.
\\:
\\: Require the module with `(library.use [queue])`.

(package queue [maybe.t @none @some]

(datatype t-internal
  ______________________
  (absvector 3) : (- (t A));

  Queue : (t A);
  ______________________
  (address-> Queue 0 #tag) : (t A);

  Queue : (t A);
  Front : (list A);
  ______________________
  (address-> Queue 1 Front) : (t A);

  Queue : (t A);
  Rear : (list A);
  ______________________
  (address-> Queue 2 Rear) : (t A);

  Queue : (t A);
  ______________________
  (<-address Queue 1) : (list A);

  Queue : (t A);
  ______________________
  (<-address Queue 2) : (list A);)

\\: == Construction

(define make_
  { (list A) --> (list A) --> (t A) }
  Front Rear -> (init-rear_ (init-front_ (address-> (absvector 3) 0 #tag)
                                         Front)
                            Rear))

(define init-front_
  { (t A) --> (list A) --> (t A) }
  Queue Front -> (address-> Queue 1 Front))

(define init-rear_
  { (t A) --> (list A) --> (t A) }
  Queue Rear -> (address-> Queue 2 Rear))

(define front_
  { (t A) --> (list A) }
  Queue -> (<-address Queue 1))

(define rear_
  { (t A) --> (list A) }
  Queue -> (<-address Queue 2))

(define normalize_
  { (list A) --> (list A) --> (t A) }
  [] Rear -> (make_ (reverse Rear) [])
  Front Rear -> (make_ Front Rear))

\\: `(queue.empty)` returns an empty queue.
(define empty
  { --> (t A) }
  -> (make_ [] []))

\\: `(queue.singleton Value)` returns a queue containing only `Value`.
(define singleton
  { A --> (t A) }
  Value -> (make_ [Value] []))

\\: `(queue.of-list Values)` returns a queue whose dequeue order is the order
\\: of `Values`. The input list is not modified.
(define of-list
  { (list A) --> (t A) }
  Values -> (make_ Values []))

\\: == Predicates

\\: `(queue.empty? Queue)` returns `true` exactly when `Queue` contains no
\\: values.
(define queue.empty?
  { (t A) --> boolean }
  Queue -> (and (= [] (front_ Queue))
                (= [] (rear_ Queue))))

\\: == Insertion

\\: `(queue.snoc Queue Value)` returns a new queue containing the values of
\\: `Queue` followed by `Value`. `Queue` is unchanged, and the operation takes
\\: constant time.
(define snoc
  { (t A) --> A --> (t A) }
  Queue Value -> (normalize_ (front_ Queue) [Value | (rear_ Queue)]))

\\: == Observation and removal

\\: `(queue.peek Queue)` returns `(@some Value)` for the next value that would
\\: be removed, or `(@none)` when `Queue` is empty. `Queue` is unchanged.
(define peek
  { (t A) --> (maybe.t A) }
  Queue -> (peek_ (front_ Queue) (rear_ Queue)))

(define peek_
  { (list A) --> (list A) --> (maybe.t A) }
  [] [] -> (@none)
  [] Rear -> (peek_ (reverse Rear) [])
  [Value | _] _ -> (@some Value))

\\: `(queue.uncons Queue)` returns `(@none)` when `Queue` is empty. Otherwise
\\: it returns `(@some (@p Value Rest))`, where `Value` is the oldest enqueued
\\: value and `Rest` is a new queue containing the remaining values. `Queue`
\\: itself is unchanged. Removing the last value in the front list normalizes
\\: the queue by reversing its rear list, taking time linear in the rear length.
(define uncons
  { (t A) --> (maybe.t (A * (t A))) }
  Queue -> (uncons_ (front_ Queue) (rear_ Queue)))

(define uncons_
  { (list A) --> (list A) --> (maybe.t (A * (t A))) }
  [] [] -> (@none)
  [] Rear -> (uncons_ (reverse Rear) [])
  [Value | Front] Rear -> (@some (@p Value (normalize_ Front Rear))))

\\: == Conversion

\\: `(queue.to-list Queue)` returns all values in dequeue order. `Queue` is
\\: unchanged.
(define to-list
  { (t A) --> (list A) }
  Queue -> (append (front_ Queue) (reverse (rear_ Queue))))

(define #tag
  { (t A) --> string }
  Queue -> (make-string "(queue ~S)" (to-list Queue)))

(preclude [t-internal])

)
