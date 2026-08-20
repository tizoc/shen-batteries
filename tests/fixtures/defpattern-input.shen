(package defpattern-guide [defpattern]

\* A small opaque cursor over a vector of bytes. The implementation is test
   support for the programmable pattern below; consumers use only its public
   constructor and accessors. *\

(datatype bits-internal
  ______________
  (absvector 3) : (- bits);

  Cursor : bits;
  ______________
  (address-> Cursor 0 #bits-tag) : bits;

  Cursor : bits;
  Bytes : (vector number);
  ______________
  (address-> Cursor 1 Bytes) : bits;

  Cursor : bits;
  Offset : number;
  ______________
  (address-> Cursor 2 Offset) : bits;

  Cursor : bits;
  ______________
  (<-address Cursor 1) : (vector number);

  Cursor : bits;
  ______________
  (<-address Cursor 2) : number;

  (absvector? X) : verified;
  ______________
  (= (<-address X 0) #bits-tag) : boolean;)

(define list-length
  { (list A) --> number }
  [] -> 0
  [_ | Xs] -> (+ 1 (list-length Xs)))

(define fill-vector
  { (list A) --> (vector A) --> number --> (vector A) }
  [] Vector _ -> Vector
  [X | Xs] Vector Position
    -> (fill-vector Xs
                    (vector-> Vector Position X)
                    (+ Position 1)))

(define bytes-vector
  { (list number) --> (vector number) }
  Bytes -> (fill-vector Bytes (vector (list-length Bytes)) 1))

(define make-bits
  { (vector number) --> number --> bits }
  Bytes Offset
    -> (address->
         (address->
           (address-> (absvector 3) 0 #bits-tag)
           1 Bytes)
         2 Offset))

(define bits
  { (list number) --> bits }
  Bytes -> (make-bits (bytes-vector Bytes) 0))

(define bits?
  { A --> boolean }
  X -> (trap-error (= (<-address X 0) #bits-tag)
                   (/. Error false))
    where (absvector? X)
  _ -> false)

(define bytes
  { bits --> (vector number) }
  Cursor -> (<-address Cursor 1))

(define offset
  { bits --> number }
  Cursor -> (<-address Cursor 2))

(define remaining
  { bits --> number }
  Cursor -> (- (* (limit (bytes Cursor)) 8) (offset Cursor)))

(define has-bits?
  { bits --> number --> boolean }
  Cursor Width -> (>= (remaining Cursor) Width))

(define locate-bit
  { number --> number --> (number * number) }
  Position Byte -> (locate-bit (- Position 8) (+ Byte 1))
    where (>= Position 8)
  Position Byte -> (@p Byte Position))

(define bit-in-byte
  { number --> number --> (list number) --> number }
  Byte 0 [Weight | _] -> (if (>= Byte Weight) 1 0)
  Byte Position [Weight | Weights]
    -> (bit-in-byte (if (>= Byte Weight) (- Byte Weight) Byte)
                    (- Position 1)
                    Weights))

(define bit-at
  { bits --> number --> number }
  Cursor Position
    -> (let Location (locate-bit (+ (offset Cursor) Position) 1)
         (bit-in-byte (<-vector (bytes Cursor) (fst Location))
                      (snd Location)
                      [128 64 32 16 8 4 2 1])))

(define peek-h
  { bits --> number --> number --> number --> number }
  _ 0 _ Value -> Value
  Cursor Width Position Value
    -> (peek-h Cursor
               (- Width 1)
               (+ Position 1)
               (+ (* Value 2) (bit-at Cursor Position))))

(define peek
  { bits --> number --> number }
  Cursor Width -> (peek-h Cursor Width 0 0))

(define bit-drop
  { bits --> number --> bits }
  Cursor Width -> (make-bits (bytes Cursor) (+ (offset Cursor) Width)))

\\ Normalize public polyadic syntax into one recursively typed internal shape.
(defmacro bits-pattern-macro
  [@bits Rest] -> Rest
  [@bits Width Pattern | More]
    -> [@bit Width Pattern [@bits | More]]
  [@bits | _]
    -> (simple-error
         "@bits expects width/pattern pairs followed by a rest pattern"))

(datatype bit-pattern
  Width : number;
  Pattern : number;
  Rest : bits;
  ==================================
  (@bit Width Pattern Rest) : bits;)

(defpattern bit-pattern-handler
  Self Is? Assign [@bit Width Pattern Rest]
    -> (do (Is? [and [bits? Self]
                       [has-bits? Self Width]])
           (Assign Pattern [peek Self Width])
           (Assign Rest [bit-drop Self Width])
           handled))

(preclude [bits-internal])

)
