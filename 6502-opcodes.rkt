#lang racket
(provide (all-defined-out))

(module opcode racket
  (struct opcode
    (mnemonic
     bytes
     mode
     register
     cycles))
  
  (define (symbol-of-length-3? a-sym)
    (= 3 (string-length (symbol->string a-sym))))
  (define cycles/c
    (and/c positive? natural-number/c))
  (define mnemonic/c
    (and/c symbol? symbol-of-length-3?))
  (define mode/c
    (one-of/c 'indirect 'zp 'implied 'immediate 'absolute 'relative 'acc))
  (define bytes/c
    (one-of/c 1 2 3))
  (define register/c
    (one-of/c 'x 'y 'none))
  
  (provide (contract-out
            [struct opcode ((mnemonic mnemonic/c)
                            (bytes bytes/c)
                            (mode mode/c)
                            (register register/c)
                            (cycles cycles/c))]
            [mk-opcode (->* (mnemonic/c
                             bytes/c
                             #:cycles cycles/c
                             #:mode mode/c)
                            (#:register register/c)
                            opcode?)]))
  
  (define (mk-opcode code
                     bytes
                     #:mode [mode 'none]
                     #:register [reg 'none]
                     #:cycles cycles)
    (opcode code bytes mode reg cycles)))

(require 'opcode)

(define opcodes
  (hash #x00 (mk-opcode 'brk 1 #:cycles 2 #:mode 'implied)
        #x01 (mk-opcode 'ora 2 #:cycles 6 #:mode 'indirect #:register 'x)
        #x05 (mk-opcode 'ora 2 #:cycles 3 #:mode 'zp)
        #x06 (mk-opcode 'asl 2 #:cycles 5 #:mode 'zp)
        #x08 (mk-opcode 'php 1 #:cycles 3 #:mode 'implied)
        #x09 (mk-opcode 'ora 2 #:cycles 2 #:mode 'immediate)
        #x0a (mk-opcode 'asl 1 #:cycles 2 #:mode 'acc)
        #x0d (mk-opcode 'ora 3 #:cycles 4 #:mode 'absolute)
        #x0e (mk-opcode 'asl 3 #:cycles 6 #:mode 'absolute)
        #x10 (mk-opcode 'bpl 2 #:cycles 2 #:mode 'relative)               ; +1 cycle if branch taken, +1 cycle if branch crosses page boundary
        #x11 (mk-opcode 'ora 2 #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x15 (mk-opcode 'ora 2 #:cycles 4 #:mode 'zp #:register 'x)
        #x16 (mk-opcode 'asl 2 #:cycles 6 #:mode 'zp #:register 'x)
        #x18 (mk-opcode 'clc 1 #:cycles 2 #:mode 'implied)
        #x19 (mk-opcode 'ora 3 #:cycles 4 #:mode 'absolute) ; +1 cycle if page boundary crossed
        #x1d (mk-opcode 'ora 3 #:cycles 4 #:mode 'absolute) ; +1 cycle if page boundary crossed
        #x1e (mk-opcode 'asl 3 #:cycles 7 #:mode 'absolute #:register 'x)
        #x20 (mk-opcode 'jsr 3 #:cycles 6 #:mode 'absolute)
        #x21 (mk-opcode 'and 2 #:cycles 6 #:mode 'indirect #:register 'x)
        #x24 (mk-opcode 'bit 2 #:cycles 3 #:mode 'zp)
        #x25 (mk-opcode 'and 2 #:cycles 3 #:mode 'zp)
        #x26 (mk-opcode 'rol 2 #:cycles 5 #:mode 'zp)
        #x28 (mk-opcode 'plp 1 #:cycles 4 #:mode 'implied)
        #x29 (mk-opcode 'and 2 #:cycles 2 #:mode 'immediate)
        #x2a (mk-opcode 'rol 1 #:cycles 2 #:mode 'acc)
        #x2c (mk-opcode 'bit 3 #:cycles 4 #:mode 'absolute)
        #x2d (mk-opcode 'and 3 #:cycles 4 #:mode 'absolute)
        #x2e (mk-opcode 'rol 3 #:cycles 6 #:mode 'absolute)
        #x30 (mk-opcode 'bmi 2 #:cycles 2 #:mode 'relative)               ; see bpl
        #x31 (mk-opcode 'and 2 #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x35 (mk-opcode 'and 2 #:cycles 4 #:mode 'zp #:register 'x)
        #x36 (mk-opcode 'rol 2 #:cycles 6 #:mode 'zp #:register 'x)
        #x38 (mk-opcode 'sec 1 #:cycles 2 #:mode 'implied)
        #x39 (mk-opcode 'and 3 #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #x3d (mk-opcode 'and 3 #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #x3e (mk-opcode 'rol 3 #:cycles 7 #:mode 'absolute #:register 'x)
        #x40 (mk-opcode 'rti 1 #:cycles 6 #:mode 'implied)
        #x41 (mk-opcode 'eor 2 #:cycles 6 #:mode 'indirect #:register 'x)
        #x45 (mk-opcode 'eor 2 #:cycles 3 #:mode 'zp)
        #x46 (mk-opcode 'lsr 2 #:cycles 5 #:mode 'zp)
        #x48 (mk-opcode 'pha 1 #:cycles 3 #:mode 'implied)
        #x49 (mk-opcode 'eor 2 #:cycles 2 #:mode 'immediate)
        #x4a (mk-opcode 'lsr 1 #:cycles 2 #:mode 'acc)
        #x4c (mk-opcode 'jmp 3 #:cycles 3 #:mode 'absolute)
        #x4d (mk-opcode 'eor 3 #:cycles 4 #:mode 'absolute)
        #x43 (mk-opcode 'lsr 3 #:cycles 6 #:mode 'absolute)
        #x50 (mk-opcode 'bvc 2 #:cycles 2 #:mode 'relative)               ; see bpl
        #x51 (mk-opcode 'eor 2 #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x55 (mk-opcode 'eor 2 #:cycles 4 #:mode 'zp #:register 'x)
        #x56 (mk-opcode 'lsr 2 #:cycles 6 #:mode 'zp #:register 'x)
        #x58 (mk-opcode 'cli 1 #:cycles 2 #:mode 'implied)
        #x59 (mk-opcode 'eor 3 #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #x5d (mk-opcode 'eor 3 #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #x5e (mk-opcode 'lsr 3 #:cycles 7 #:mode 'absolute #:register 'x)
        #x60 (mk-opcode 'rts 1 #:cycles 6 #:mode 'implied)
        #x61 (mk-opcode 'adc 2 #:cycles 6 #:mode 'indirect #:register 'x)
        #x65 (mk-opcode 'adc 2 #:cycles 5 #:mode 'zp)
        #x66 (mk-opcode 'ror 2 #:cycles 5 #:mode 'zp)
        #x68 (mk-opcode 'pla 1 #:cycles 4 #:mode 'implied)
        #x69 (mk-opcode 'adc 2 #:cycles 2 #:mode 'immediate)
        #x6a (mk-opcode 'ror 1 #:cycles 2 #:mode 'acc)
        #x6c (mk-opcode 'jmp 3 #:cycles 5 #:mode 'indirect)
        #x6d (mk-opcode 'adc 3 #:cycles 4 #:mode 'absolute)
        #x6e (mk-opcode 'ror 3 #:cycles 6 #:mode 'absolute)
        #x70 (mk-opcode 'bvs 2 #:cycles 2 #:mode 'relative)               ; see bpl
        #x71 (mk-opcode 'adc 2 #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x75 (mk-opcode 'adc 2 #:cycles 4 #:mode 'zp #:register 'x)
        #x76 (mk-opcode 'ror 2 #:cycles 6 #:mode 'zp #:register 'x)
        #x78 (mk-opcode 'sei 1 #:cycles 2 #:mode 'implied)
        #x79 (mk-opcode 'adc 3 #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #x7d (mk-opcode 'adc 3 #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #x7e (mk-opcode 'ror 3 #:cycles 7 #:mode 'absolute #:register 'x)
        #x81 (mk-opcode 'sta 2 #:cycles 6 #:mode 'indirect #:register 'x)
        #x84 (mk-opcode 'sty 2 #:cycles 3 #:mode 'zp)
        #x85 (mk-opcode 'sta 2 #:cycles 3 #:mode 'zp)
        #x86 (mk-opcode 'stx 2 #:cycles 3 #:mode 'zp)
        #x88 (mk-opcode 'dey 1 #:cycles 2 #:mode 'implied)
        #x8a (mk-opcode 'txa 1 #:cycles 2 #:mode 'implied)
        #x8c (mk-opcode 'sty 3 #:cycles 4 #:mode 'absolute)
        #x8d (mk-opcode 'sta 3 #:cycles 4 #:mode 'absolute)
        #x8e (mk-opcode 'stx 3 #:cycles 4 #:mode 'absolute)
        #x90 (mk-opcode 'bcc 2 #:cycles 2 #:mode 'relative) ; see bpl
        #x91 (mk-opcode 'sta 2 #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x94 (mk-opcode 'sty 2 #:cycles 4 #:mode 'zp #:register 'x)
        #x95 (mk-opcode 'sta 2 #:cycles 4 #:mode 'zp #:register 'x)
        #x96 (mk-opcode 'stx 2 #:cycles 4 #:mode 'zp #:register 'y)
        #x98 (mk-opcode 'tya 1 #:cycles 2 #:mode 'implied)
        #x99 (mk-opcode 'sta 3 #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #x9a (mk-opcode 'txs 1 #:cycles 2 #:mode 'implied)
        #x9d (mk-opcode 'sta 3 #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xa0 (mk-opcode 'ldy 2 #:cycles 2 #:mode 'immediate)
        ))