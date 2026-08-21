\\ Copyright (c) 2026 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package record []

(define address-readable?
  X N -> (trap-error (do (<-address X N) true)
                         (/. Error false))
    where (absvector? X)
  _ _ -> false)

(define address-value=?
  X N Expected -> (trap-error (= (<-address X N) Expected)
                              (/. Error false))
    where (absvector? X)
  _ _ _ -> false)

(define portable-intern
  UID MakeName Fields -> [UID MakeName Fields])

(define portable-fill
  Record [] _ -> Record
  Record [Value | Values] Index
    -> (do (address-> Record Index Value)
           (portable-fill Record Values (+ Index 1))))

(define portable-make
  Schema Values
    -> (let Record (absvector (+ 2 (length Values)))
         (do (address-> Record 0 portable-print)
             (address-> Record 1 Schema)
             (portable-fill Record Values 2))))

(define portable?
  Schema X
    -> (let Last (+ 1 (length (hd (tl (tl Schema)))))
         (and (address-value=? X 0 portable-print)
              (and (address-value=? X 1 Schema)
                   (and (address-readable? X Last)
                        (not (address-readable? X (+ Last 1))))))))

(define portable-ref
  _ Record Index -> (<-address Record (+ Index 2)))

(define portable-print-fields
  [] _ _ -> ")"
  [Field | Fields] Record Index
    -> (@s " "
         (@s (str Field)
          (@s " <- "
           (@s (make-string "~S" (<-address Record Index))
            (@s ";"
                (portable-print-fields Fields Record (+ Index 1))))))))

(define portable-print
  Record
    -> (let Schema (<-address Record 1)
            MakeName (hd (tl Schema))
            Fields (hd (tl (tl Schema)))
         (@s "(" (@s (str MakeName)
                       (portable-print-fields Fields Record 2)))))

)
