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

(define (byte/c x)
  (flat-named-contract
   'byte
   (and/c natural-number/c (between/c 0 #xff))))

(define/contract (vector-length= n)
  (natural-number/c . -> . flat-contract?)
  (flat-named-contract
   'vector-length=
   (lambda (v)
     (and (vector? v)
          (eqv? (vector-length v) 16)))))

(define/contract (bytes->ines-header bytev)
  ((and/c (vectorof byte/c) (vector-length= 16)) . -> . ines-header?)
  (ines-header (vector-ref bytev 4)
               (vector-ref bytev 8)
               (vector-ref bytev 5)
               (vector-ref bytev 6)
               (vector-ref bytev 7)
               (vector-ref bytev 9)
               (vector-ref bytev 10)))

(define/contract (tv-system ines-header)
  (ines-header? . -> . (one-of/c 'pal 'ntsc))
  (let ([tv-system-flag (bitwise-bit-set? (ines-header-flags9 ines-header) 0)])
    (if tv-system-flag
        'pal
        'ntsc)))

(define/contract (mapper-number ines-header)
  (ines-header? . -> . byte/c)
  (let ([lower-nibble (bitwise-and #xf0 (ines-header-flags6 ines-header))]
        [upper-nibble (bitwise-and #xf0 (ines-header-flags7 ines-header))])
    (+ (arithmetic-shift lower-nibble -4)
       upper-nibble)))

(define/contract (has-trainer? ines-header)
  (ines-header? . -> . boolean?)
  (bitwise-bit-set? (ines-header-flags6 ines-header) 2))

(define/contract (vs-unisystem? ines-header)
  (ines-header? . -> . boolean?)
  (bitwise-bit-set? (ines-header-flags7 ines-header) 0))