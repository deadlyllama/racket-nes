#lang racket
(provide (all-defined-out))

(struct opcode
  (mnemonic
   bytes
   mode
   register
   cycles))

(define (mk-opcode code
                   bytes
                   #:mode [mode 'none]
                   #:register [reg 'none]
                   #:cycles cycles)
  (opcode code bytes mode reg cycles))

(define opcodes
  (hash #x00 (mk-opcode 'brk 1 #:cycles 2)
        #x01 (mk-opcode 'ora 2 #:cycles 6 #:mode 'indirect #:register 'x)
        #x05 (mk-opcode 'ora 2 #:cycles 3 #:mode 'zp)
        #x06 (mk-opcode 'asl 2 #:cycles 5 #:mode 'zp)
        #x08 (mk-opcode 'php 1 #:cycles 3 #:mode 'implied)
        #x09 (mk-opcode 'ora 2 #:cycles 2 #:mode 'immediate)
        #x0a (mk-opcode 'asl 1 #:cycles 2 #:mode 'acc)
        #x0d (mk-opcode 'ora 3 #:cycles 4 #:mode 'absolute)
        #x0e (mk-opcode 'asl 3 #:cycles 6 #:mode 'absolute)
        #x10 (mk-opcode 'bpl 2 #:cycles 'variable #:mode 'relative)        ; 2+ cycles
        #x11 (mk-opcode 'ora 2 #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x15 (mk-opcode 'ora 2 #:cycles 4 #:mode 'zp #:register 'x)
        #x16 (mk-opcode 'asl 2 #:cycles 6 #:mode 'zp #:register 'x)
        #x18 (mk-opcode 'clc 1 #:cycles 2 #:mode 'implied)
        #x19 (mk-opcode 'ora 3 #:cycles 4 #:mode 'absolute) ; +1 cycle if page boundary crossed
        #x1d (mk-opcode 'ora 3 #:cycles 4 #:mode 'absolute) ; +1 cycle if page boundary crossed
        #x1e (mk-opcode 'asl 3 #:cycles 7 #:mode 'absolute #:register 'x)
        ))