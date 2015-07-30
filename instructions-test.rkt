#lang racket

(require rackunit)
(require "instructions.rkt")

(let ([new-state (state-set-a initial-state 7)])
  (check-equal? (state-a new-state)
                7))

(let ([new-state (state-set-p initial-state 1001)])
  (check-equal? (state-p new-state)
                1001))

(let ([start-state (state-set-a initial-state 1)]
      [end-state (state-set-a initial-state 2)])
  (check-equal? (asl start-state 1)
                end-state
                "arithmetic shift left by one"))

(let ([start-state (state-set-a initial-state #xff)]
      [end-state (struct-copy state initial-state
                              [a #xfc]
                              [p 1])])
  (check-equal? (asl start-state 2)
                end-state
                "arithmetic shift left by two, set carry"))

(let ([start-state (state-set-carry initial-state #t)]
      [end-state initial-state])
  (check-equal? (asl start-state 1)
                end-state
                "arithmetic shift left by one, unset carry"))

(let ([start-state (state-set-a initial-state 1)]
      [end-state (state-set-p initial-state 1)])
  (check-equal? (lsr start-state 1)
                end-state
                "logical shift right by one, set carry"))

(let ([start-state (state-set-a initial-state 10)]
      [end-state (state-set-a initial-state 5)])
  (check-equal? (lsr start-state 1)
                end-state
                "logical shift right by one"))

(let ([start-state (struct-copy state initial-state
                                [p 1]
                                [a 4])]
      [end-state (state-set-a initial-state 1)])
  (check-equal? (lsr start-state 2)
               end-state
               "logical shift right by two, unset carry"))
