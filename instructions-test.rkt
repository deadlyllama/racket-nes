#lang racket

(require rackunit)
(require "instructions.rkt")

(let ([new-state (state-set-reg-a initial-state 7)])
  (check-equal? (state-reg-a new-state)
                7))

(let ([start-state (state-set-reg-a initial-state 1)]
      [end-state (state-set-reg-a initial-state 2)])
  (check-equal? (asl start-state 1)
                end-state
                "arithmetic shift left by one"))

(let ([start-state (state-set-reg-a initial-state #xff)]
      [end-state (struct-copy state initial-state
                              [reg-a #xfc]
                              [carry #t])])
  (check-equal? (asl start-state 2)
                end-state
                "arithmetic shift left by two, set carry"))

(let ([start-state (state-set-carry initial-state #t)]
      [end-state initial-state])
  (check-equal? (asl start-state 1)
                end-state
                "arithmetic shift left by one, unset carry"))

(let ([start-state (state-set-reg-a initial-state 1)]
      [end-state (state-set-carry initial-state #t)])
  (check-equal? (lsr start-state 1)
                end-state
                "logical shift right by one, set carry"))

(let ([start-state (state-set-reg-a initial-state 10)]
      [end-state (state-set-reg-a initial-state 5)])
  (check-equal? (lsr start-state 1)
                end-state
                "logical shift right by one"))

(let ([start-state (struct-copy state initial-state
                                [carry #t]
                                [reg-a 4])]
      [end-state (state-set-reg-a initial-state 1)])
  (check-equal? (lsr start-state 2)
               end-state
               "logical shift right by two, unset carry"))
