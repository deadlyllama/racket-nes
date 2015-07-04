#lang racket

(define ines-header-size 16)
(define trainer-size 512)
(define prg-rom-page-size (* 16 1024))

(define (read-ines-rom path)
  (let* ([bytes (read-rom-bytes path)]
         [header (bytes->ines-header (header-bytes bytes))]
         [prg-rom (prg-bytes header bytes)])
    (ines-rom header prg-rom)))

(define (read-rom-bytes path)
  (let* ([input (open-input-file path #:mode 'binary)]
         [bytes (read-bytes input)])
    (close-input-port input)
    bytes))

(define (read-bytes input)
  (let ([byte (read-byte input)])
    (if (eof-object? byte)
        '()
        (cons byte (read-bytes input)))))

; TODO: constraint for bytes (should be list bytes of size 16)
(define (bytes->ines-header bytev)
  (ines-header (vector-ref bytev 4)
               (vector-ref bytev 8)
               (vector-ref bytev 5)
               (vector-ref bytev 6)
               (vector-ref bytev 7)
               (vector-ref bytev 9)
               (vector-ref bytev 10)))

(define (prg-bytes ines-header bytes)
  (let* ([prg-rom-bytes (* prg-rom-page-size
                           (ines-header-prg-rom-size-x16k ines-header))]
         [ignored-bytes (if (has-trainer? ines-header)
                            (+ ines-header-size
                               trainer-size)
                            ines-header-size)]
         [prg-bytes-start (drop bytes ignored-bytes)])
    (vector->immutable-vector
     (list->vector
      (take bytes prg-rom-bytes)))))

(define (header-bytes bytes)
  (vector->immutable-vector
   (list->vector
    (take bytes ines-header-size))))

(struct ines-rom
  (header
   prg-rom)
  #:prefab)

(struct ines-header
  (prg-rom-size-x16k
   prg-ram-size-x8k
   chr-rom-size-x8k
   flags6
   flags7
   flags9
   flags10)
  #:prefab)

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