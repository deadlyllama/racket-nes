#lang racket

(struct state
  (x
   y
   a
   p
   pc
   sp)
  #:prefab)

(define initial-state
  (state 0 0 0 0 #x8000 #x01FF))

(define (nop state)
  state)

(define (asl a-state a-value amount)
  (let* ([shifted (arithmetic-shift a-value amount)]
         [masked (bitwise-and #xff shifted)]
         [flags (state-p a-state)]
         [new-flags (if (bitwise-bit-set? shifted 8)
                        (bitwise-ior flags 1)
                        (bitwise-and flags (bitwise-not 1)))])
    (struct-copy state a-state
                 [a masked]
                 [p new-flags])))

(define instructions
  (hash 'nop nop
        'asl asl))
