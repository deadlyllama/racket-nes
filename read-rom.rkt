#lang racket
(require "ines-header.rkt")
(require "6502-opcodes.rkt")

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
      (take prg-bytes-start prg-rom-bytes)))))

(define (header-bytes bytes)
  (vector->immutable-vector
   (list->vector
    (take bytes ines-header-size))))

(struct op
  (hex
   code
   param)
  #:prefab)

(define (disassemble prg-rom location)
  (if (>= location (vector-length prg-rom))
      empty-stream
      (let* ([current (vector-ref prg-rom location)]
             [opcode (hash-ref opcodes current)]
             [param-length (sub1 (instruction-length opcode))]
             [hex (format "~x" current)])
        (if (zero? param-length)
            (stream-cons (op hex opcode 'none)
                  (disassemble prg-rom (add1 location)))
            (let* ([param-start (add1 location)]
                   [next-location (+ param-start param-length)])
              (stream-cons (op hex opcode (vector-copy prg-rom param-start next-location))
                  (disassemble prg-rom next-location)))))))

(struct ines-rom
  (header
   prg-rom)
  #:prefab)