(package defpattern-guide []

(define read-small-frame
  { bits --> (list number) }
  (@bits 1 Fin
         3 0
         4 Opcode
         1 Masked
         7 Length
         Rest)
    -> [Fin Opcode Masked Length (remaining Rest)]
  _ -> [])

(define read-short-prefix
  { bits --> (list number) }
  (@bits 1 Flag
         3 Kind
         Rest)
    -> [Flag Kind (remaining Rest)]
  _ -> [])

)
