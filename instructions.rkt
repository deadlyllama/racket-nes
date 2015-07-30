#lang racket
(provide (all-defined-out))

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

(define (asl a-state amount)
  (let* ([current (state-a a-state)]
         [shifted (arithmetic-shift current amount)]
         [masked (bitwise-and #xff shifted)]
         [carry (bitwise-bit-set? shifted 8)]
         [new-state (state-set-carry a-state carry)])
    (state-set-a new-state masked)))

(define (lsr a-state a-value amount)
  (let* ([shifted (arithmetic-shift a-value (- amount))]
         [carry (bitwise-bit-set? a-value 0)]
         [new-state (state-set-carry a-state carry)])
    (state-set-a new-state shifted)))

(define (state-set-carry a-state carry)
  (let* ([flags (state-p a-state)]
         [new-flags (if carry
                        (bitwise-ior flags 1)
                        (bitwise-and flags (bitwise-not 1)))])
    (state-set-p a-state new-flags)))

(define (state-set-a a-state new-accumulator)
  (struct-copy state a-state
               [a new-accumulator]))

(define (state-set-p a-state new-flags)
  (struct-copy state a-state
               [p new-flags]))

(define instructions
  (hash 'nop nop
        'asl asl))
