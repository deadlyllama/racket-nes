#lang racket
(provide (all-defined-out))

(struct ines-header
  (prg-rom-size-x16k
   prg-ram-size-x8k
   chr-rom-size-x8k
   flags6
   flags7
   flags9
   flags10)
  #:prefab)

; TODO: constraint for bytes (should be byte vector of size 16)
(define (bytes->ines-header bytev)
  (ines-header (vector-ref bytev 4)
               (vector-ref bytev 8)
               (vector-ref bytev 5)
               (vector-ref bytev 6)
               (vector-ref bytev 7)
               (vector-ref bytev 9)
               (vector-ref bytev 10)))

(define (tv-system ines-header)
  (let ([tv-system-flag (bitwise-bit-set? (ines-header-flags9 ines-header) 0)])
    (if tv-system-flag
        'pal
        'ntsc)))

(define (mapper-number ines-header)
  (let ([lower-nibble (bitwise-and #xf0 (ines-header-flags6 ines-header))]
        [upper-nibble (bitwise-and #xf0 (ines-header-flags7 ines-header))])
    (+ (arithmetic-shift lower-nibble -4)
       upper-nibble)))

(define (has-trainer? ines-header)
  (bitwise-bit-set? (ines-header-flags6 ines-header) 2))

(define (vs-unisystem? ines-header)
  (bitwise-bit-set? (ines-header-flags7 ines-header) 0))