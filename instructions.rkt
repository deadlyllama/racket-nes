#lang racket
(provide (all-defined-out))

(struct state
  (reg-x
   reg-y
   reg-a
   carry
   pc
   sp)
  #:prefab)

(define initial-state
  (state 0 0 0 #f #x8000 #x01FF))

(define (nop state)
  state)

(define (asl a-state amount)
  (let* ([current (state-reg-a a-state)]
         [shifted (arithmetic-shift current amount)]
         [masked (bitwise-and #xff shifted)]
         [carry (bitwise-bit-set? shifted 8)]
         [new-state (state-set-carry a-state carry)])
    (state-set-reg-a new-state masked)))

(define (lsr a-state amount)
  (let* ([current (state-reg-a a-state)]
         [shifted (arithmetic-shift current (- amount))]
         [carry (bitwise-bit-set? current 0)]
         [new-state (state-set-carry a-state carry)])
    (state-set-reg-a new-state shifted)))

(define (state-set-carry a-state new-carry)
  (struct-copy state a-state
               [carry new-carry]))

(define (state-set-reg-a a-state new-accumulator)
  (struct-copy state a-state
               [reg-a new-accumulator]))

(define instructions
  (hash 'nop nop
        'asl asl
        'lsr lsr))
