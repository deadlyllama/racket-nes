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
        #xa1 (mk-opcode 'lda 2 #:cycles 6 #:mode 'indirect #:register 'x)
        #xa2 (mk-opcode 'ldx 2 #:cycles 2 #:mode 'immediate)
        #xa4 (mk-opcode 'ldy 2 #:cycles 3 #:mode 'zp)
        #xa5 (mk-opcode 'lda 2 #:cycles 3 #:mode 'zp)
        #xa6 (mk-opcode 'ldx 2 #:cycles 3 #:mode 'zp)
        #xa8 (mk-opcode 'tay 1 #:cycles 2 #:mode 'implied)
        #xa9 (mk-opcode 'lda 2 #:cycles 2 #:mode 'immediate)
        #xaa (mk-opcode 'tax 1 #:cycles 2 #:mode 'implied)
        #xac (mk-opcode 'ldy 3 #:cycles 4 #:mode 'absolute)
        #xad (mk-opcode 'lda 3 #:cycles 4 #:mode 'absolute)
        #xae (mk-opcode 'ldx 3 #:cycles 4 #:mode 'absolute)
        #xb0 (mk-opcode 'bcs 2 #:cycles 2 #:mode 'relative) ; see bpl
        #xb1 (mk-opcode 'lda 2 #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #xb4 (mk-opcode 'ldy 2 #:cycles 4 #:mode 'zp #:register 'x)
        #xb5 (mk-opcode 'lda 2 #:cycles 4 #:mode 'zp #:register 'x)
        #xb6 (mk-opcode 'ldx 2 #:cycles 4 #:mode 'zp #:register 'x)
        #xb8 (mk-opcode 'clv 1 #:cycles 2 #:mode 'implied)
        #xb9 (mk-opcode 'lda 3 #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #xba (mk-opcode 'tsx 1 #:cycles 2 #:mode 'implied)
        #xbc (mk-opcode 'ldy 3 #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xbd (mk-opcode 'lda 3 #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xbe (mk-opcode 'ldx 3 #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xc0 (mk-opcode 'cpy 2 #:cycles 2 #:mode 'immediate)
        #xc1 (mk-opcode 'cmp 2 #:cycles 6 #:mode 'indirect #:register 'x) ; +1 cycle if page boundary crossed
        #xc4 (mk-opcode 'cpy 2 #:cycles 3 #:mode 'zp)
        #xc5 (mk-opcode 'cmp 2 #:cycles 3 #:mode 'zp)
        #xc6 (mk-opcode 'dec 2 #:cycles 5 #:mode 'zp)
        #xc8 (mk-opcode 'iny 1 #:cycles 2 #:mode 'implied)
        #xc9 (mk-opcode 'cmp 2 #:cycles 2 #:mode 'immediate)
        #xca (mk-opcode 'dex 1 #:cycles 2 #:mode 'implied)
        #xcc (mk-opcode 'cpy 3 #:cycles 4 #:mode 'absolute)
        #xcd (mk-opcode 'cmp 3 #:cycles 4 #:mode 'absolute)
        #xce (mk-opcode 'dec 3 #:cycles 6 #:mode 'absolute)
        #xd0 (mk-opcode 'bne 2 #:cycles 2 #:mode 'relative) ; see bpl
        #xd1 (mk-opcode 'cmp 2 #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #xd5 (mk-opcode 'cmp 2 #:cycles 4 #:mode 'zp #:register 'x)
        #xd6 (mk-opcode 'dec 2 #:cycles 6 #:mode 'zp #:register 'x)
        #xd8 (mk-opcode 'cld 1 #:cycles 2 #:mode 'implied)
        #xd9 (mk-opcode 'cmp 3 #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #xdd (mk-opcode 'cmp 3 #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xde (mk-opcode 'dec 3 #:cycles 7 #:mode 'absolute #:register 'x)
        #xe0 (mk-opcode 'cpx 2 #:cycles 2 #:mode 'immediate)
        #xe1 (mk-opcode 'sbc 2 #:cycles 6 #:mode 'indirect #:register 'x)
        #xe4 (mk-opcode 'cpx 2 #:cycles 3 #:mode 'zp)
        #xe5 (mk-opcode 'sbc 2 #:cycles 3 #:mode 'zp)
        #xe6 (mk-opcode 'inc 2 #:cycles 5 #:mode 'zp)
        #xe8 (mk-opcode 'inx 1 #:cycles 2 #:mode 'implied)
        #xe9 (mk-opcode 'sbc 2 #:cycles 2 #:mode 'immediate)
        #xea (mk-opcode 'nop 1 #:cycles 2 #:mode 'implied)
        #xec (mk-opcode 'cpx 3 #:cycles 4 #:mode 'absolute)
        #xed (mk-opcode 'sbc 3 #:cycles 4 #:mode 'absolute)
        #xee (mk-opcode 'inc 3 #:cycles 6 #:mode 'absolute)
        #xf0 (mk-opcode 'beq 1 #:cycles 2 #:mode 'relative) ; see bpl
        #xf1 (mk-opcode 'sbc 2 #:cycles 5 #:mode 'indirect #:register 'y)
        #xf5 (mk-opcode 'sbc 2 #:cycles 5 #:mode 'zp #:register 'x)
        #xf6 (mk-opcode 'inc 2 #:cycles 6 #:mode 'zp #:register 'x)
        #xf8 (mk-opcode 'sed 1 #:cycles 2 #:mode 'implied)
        #xf9 (mk-opcode 'sbc 3 #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #xfd (mk-opcode 'sbc 3 #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xfe (mk-opcode 'inc 3 #:cycles 7 #:mode 'absolute #:register 'x)
        ))