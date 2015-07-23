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
         [carry (if (bitwise-bit-set? shifted 8)
                   1
                   0)]
         [flags (state-p a-state)]
         [mask (bitwise-ior  #xfe carry)]
         [new-flags (bitwise-and flags mask)])
    (struct-copy state a-state
                 [a masked]
                 [p new-flags])))

(define instructions
  (hash 'nop nop
        'asl asl))
