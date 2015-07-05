#lang racket
(provide (all-defined-out))

(module opcode racket
  (struct opcode
    (mnemonic
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
                            (mode mode/c)
                            (register register/c)
                            (cycles cycles/c))]
            [mk-opcode (->* (mnemonic/c
                             #:cycles cycles/c
                             #:mode mode/c)
                            (#:register register/c)
                            opcode?)]
            [instruction-length (opcode? . -> . bytes/c)]))

  (define (mk-opcode code
                     #:mode [mode 'none]
                     #:register [reg 'none]
                     #:cycles cycles)
    (opcode code mode reg cycles))

  (define (instruction-length opcode)
    (let ([mode (opcode-mode opcode)])
      (cond
        ((eq? mode 'absolute) 3)
        ((or (eq? mode 'implied) (eq? mode 'acc)) 1)
        (else 2)))))

(require 'opcode)

(define opcodes
  (hash #x00 (mk-opcode 'brk #:cycles 2 #:mode 'implied)
        #x01 (mk-opcode 'ora #:cycles 6 #:mode 'indirect #:register 'x)
        #x05 (mk-opcode 'ora #:cycles 3 #:mode 'zp)
        #x06 (mk-opcode 'asl #:cycles 5 #:mode 'zp)
        #x08 (mk-opcode 'php #:cycles 3 #:mode 'implied)
        #x09 (mk-opcode 'ora #:cycles 2 #:mode 'immediate)
        #x0a (mk-opcode 'asl #:cycles 2 #:mode 'acc)
        #x0d (mk-opcode 'ora #:cycles 4 #:mode 'absolute)
        #x0e (mk-opcode 'asl #:cycles 6 #:mode 'absolute)
        #x10 (mk-opcode 'bpl #:cycles 2 #:mode 'relative)               ; +1 cycle if branch taken, +1 cycle if branch crosses page boundary
        #x11 (mk-opcode 'ora #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x15 (mk-opcode 'ora #:cycles 4 #:mode 'zp #:register 'x)
        #x16 (mk-opcode 'asl #:cycles 6 #:mode 'zp #:register 'x)
        #x18 (mk-opcode 'clc #:cycles 2 #:mode 'implied)
        #x19 (mk-opcode 'ora #:cycles 4 #:mode 'absolute) ; +1 cycle if page boundary crossed
        #x1d (mk-opcode 'ora #:cycles 4 #:mode 'absolute) ; +1 cycle if page boundary crossed
        #x1e (mk-opcode 'asl #:cycles 7 #:mode 'absolute #:register 'x)
        #x20 (mk-opcode 'jsr #:cycles 6 #:mode 'absolute)
        #x21 (mk-opcode 'and #:cycles 6 #:mode 'indirect #:register 'x)
        #x24 (mk-opcode 'bit #:cycles 3 #:mode 'zp)
        #x25 (mk-opcode 'and #:cycles 3 #:mode 'zp)
        #x26 (mk-opcode 'rol #:cycles 5 #:mode 'zp)
        #x28 (mk-opcode 'plp #:cycles 4 #:mode 'implied)
        #x29 (mk-opcode 'and #:cycles 2 #:mode 'immediate)
        #x2a (mk-opcode 'rol #:cycles 2 #:mode 'acc)
        #x2c (mk-opcode 'bit #:cycles 4 #:mode 'absolute)
        #x2d (mk-opcode 'and #:cycles 4 #:mode 'absolute)
        #x2e (mk-opcode 'rol #:cycles 6 #:mode 'absolute)
        #x30 (mk-opcode 'bmi #:cycles 2 #:mode 'relative)               ; see bpl
        #x31 (mk-opcode 'and #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x35 (mk-opcode 'and #:cycles 4 #:mode 'zp #:register 'x)
        #x36 (mk-opcode 'rol #:cycles 6 #:mode 'zp #:register 'x)
        #x38 (mk-opcode 'sec #:cycles 2 #:mode 'implied)
        #x39 (mk-opcode 'and #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #x3d (mk-opcode 'and #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #x3e (mk-opcode 'rol #:cycles 7 #:mode 'absolute #:register 'x)
        #x40 (mk-opcode 'rti #:cycles 6 #:mode 'implied)
        #x41 (mk-opcode 'eor #:cycles 6 #:mode 'indirect #:register 'x)
        #x45 (mk-opcode 'eor #:cycles 3 #:mode 'zp)
        #x46 (mk-opcode 'lsr #:cycles 5 #:mode 'zp)
        #x48 (mk-opcode 'pha #:cycles 3 #:mode 'implied)
        #x49 (mk-opcode 'eor #:cycles 2 #:mode 'immediate)
        #x4a (mk-opcode 'lsr #:cycles 2 #:mode 'acc)
        #x4c (mk-opcode 'jmp #:cycles 3 #:mode 'absolute)
        #x4d (mk-opcode 'eor #:cycles 4 #:mode 'absolute)
        #x43 (mk-opcode 'lsr #:cycles 6 #:mode 'absolute)
        #x50 (mk-opcode 'bvc #:cycles 2 #:mode 'relative)               ; see bpl
        #x51 (mk-opcode 'eor #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x55 (mk-opcode 'eor #:cycles 4 #:mode 'zp #:register 'x)
        #x56 (mk-opcode 'lsr #:cycles 6 #:mode 'zp #:register 'x)
        #x58 (mk-opcode 'cli #:cycles 2 #:mode 'implied)
        #x59 (mk-opcode 'eor #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #x5d (mk-opcode 'eor #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #x5e (mk-opcode 'lsr #:cycles 7 #:mode 'absolute #:register 'x)
        #x60 (mk-opcode 'rts #:cycles 6 #:mode 'implied)
        #x61 (mk-opcode 'adc #:cycles 6 #:mode 'indirect #:register 'x)
        #x65 (mk-opcode 'adc #:cycles 5 #:mode 'zp)
        #x66 (mk-opcode 'ror #:cycles 5 #:mode 'zp)
        #x68 (mk-opcode 'pla #:cycles 4 #:mode 'implied)
        #x69 (mk-opcode 'adc #:cycles 2 #:mode 'immediate)
        #x6a (mk-opcode 'ror #:cycles 2 #:mode 'acc)
        #x6c (mk-opcode 'jmp #:cycles 5 #:mode 'indirect)
        #x6d (mk-opcode 'adc #:cycles 4 #:mode 'absolute)
        #x6e (mk-opcode 'ror #:cycles 6 #:mode 'absolute)
        #x70 (mk-opcode 'bvs #:cycles 2 #:mode 'relative)               ; see bpl
        #x71 (mk-opcode 'adc #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x75 (mk-opcode 'adc #:cycles 4 #:mode 'zp #:register 'x)
        #x76 (mk-opcode 'ror #:cycles 6 #:mode 'zp #:register 'x)
        #x78 (mk-opcode 'sei #:cycles 2 #:mode 'implied)
        #x79 (mk-opcode 'adc #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #x7d (mk-opcode 'adc #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #x7e (mk-opcode 'ror #:cycles 7 #:mode 'absolute #:register 'x)
        #x81 (mk-opcode 'sta #:cycles 6 #:mode 'indirect #:register 'x)
        #x84 (mk-opcode 'sty #:cycles 3 #:mode 'zp)
        #x85 (mk-opcode 'sta #:cycles 3 #:mode 'zp)
        #x86 (mk-opcode 'stx #:cycles 3 #:mode 'zp)
        #x88 (mk-opcode 'dey #:cycles 2 #:mode 'implied)
        #x8a (mk-opcode 'txa #:cycles 2 #:mode 'implied)
        #x8c (mk-opcode 'sty #:cycles 4 #:mode 'absolute)
        #x8d (mk-opcode 'sta #:cycles 4 #:mode 'absolute)
        #x8e (mk-opcode 'stx #:cycles 4 #:mode 'absolute)
        #x90 (mk-opcode 'bcc #:cycles 2 #:mode 'relative) ; see bpl
        #x91 (mk-opcode 'sta #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #x94 (mk-opcode 'sty #:cycles 4 #:mode 'zp #:register 'x)
        #x95 (mk-opcode 'sta #:cycles 4 #:mode 'zp #:register 'x)
        #x96 (mk-opcode 'stx #:cycles 4 #:mode 'zp #:register 'y)
        #x98 (mk-opcode 'tya #:cycles 2 #:mode 'implied)
        #x99 (mk-opcode 'sta #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #x9a (mk-opcode 'txs #:cycles 2 #:mode 'implied)
        #x9d (mk-opcode 'sta #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xa0 (mk-opcode 'ldy #:cycles 2 #:mode 'immediate)
        #xa1 (mk-opcode 'lda #:cycles 6 #:mode 'indirect #:register 'x)
        #xa2 (mk-opcode 'ldx #:cycles 2 #:mode 'immediate)
        #xa4 (mk-opcode 'ldy #:cycles 3 #:mode 'zp)
        #xa5 (mk-opcode 'lda #:cycles 3 #:mode 'zp)
        #xa6 (mk-opcode 'ldx #:cycles 3 #:mode 'zp)
        #xa8 (mk-opcode 'tay #:cycles 2 #:mode 'implied)
        #xa9 (mk-opcode 'lda #:cycles 2 #:mode 'immediate)
        #xaa (mk-opcode 'tax #:cycles 2 #:mode 'implied)
        #xac (mk-opcode 'ldy #:cycles 4 #:mode 'absolute)
        #xad (mk-opcode 'lda #:cycles 4 #:mode 'absolute)
        #xae (mk-opcode 'ldx #:cycles 4 #:mode 'absolute)
        #xb0 (mk-opcode 'bcs #:cycles 2 #:mode 'relative) ; see bpl
        #xb1 (mk-opcode 'lda #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #xb4 (mk-opcode 'ldy #:cycles 4 #:mode 'zp #:register 'x)
        #xb5 (mk-opcode 'lda #:cycles 4 #:mode 'zp #:register 'x)
        #xb6 (mk-opcode 'ldx #:cycles 4 #:mode 'zp #:register 'x)
        #xb8 (mk-opcode 'clv #:cycles 2 #:mode 'implied)
        #xb9 (mk-opcode 'lda #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #xba (mk-opcode 'tsx #:cycles 2 #:mode 'implied)
        #xbc (mk-opcode 'ldy #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xbd (mk-opcode 'lda #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xbe (mk-opcode 'ldx #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xc0 (mk-opcode 'cpy #:cycles 2 #:mode 'immediate)
        #xc1 (mk-opcode 'cmp #:cycles 6 #:mode 'indirect #:register 'x) ; +1 cycle if page boundary crossed
        #xc4 (mk-opcode 'cpy #:cycles 3 #:mode 'zp)
        #xc5 (mk-opcode 'cmp #:cycles 3 #:mode 'zp)
        #xc6 (mk-opcode 'dec #:cycles 5 #:mode 'zp)
        #xc8 (mk-opcode 'iny #:cycles 2 #:mode 'implied)
        #xc9 (mk-opcode 'cmp #:cycles 2 #:mode 'immediate)
        #xca (mk-opcode 'dex #:cycles 2 #:mode 'implied)
        #xcc (mk-opcode 'cpy #:cycles 4 #:mode 'absolute)
        #xcd (mk-opcode 'cmp #:cycles 4 #:mode 'absolute)
        #xce (mk-opcode 'dec #:cycles 6 #:mode 'absolute)
        #xd0 (mk-opcode 'bne #:cycles 2 #:mode 'relative) ; see bpl
        #xd1 (mk-opcode 'cmp #:cycles 5 #:mode 'indirect #:register 'y) ; +1 cycle if page boundary crossed
        #xd5 (mk-opcode 'cmp #:cycles 4 #:mode 'zp #:register 'x)
        #xd6 (mk-opcode 'dec #:cycles 6 #:mode 'zp #:register 'x)
        #xd8 (mk-opcode 'cld #:cycles 2 #:mode 'implied)
        #xd9 (mk-opcode 'cmp #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #xdd (mk-opcode 'cmp #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xde (mk-opcode 'dec #:cycles 7 #:mode 'absolute #:register 'x)
        #xe0 (mk-opcode 'cpx #:cycles 2 #:mode 'immediate)
        #xe1 (mk-opcode 'sbc #:cycles 6 #:mode 'indirect #:register 'x)
        #xe4 (mk-opcode 'cpx #:cycles 3 #:mode 'zp)
        #xe5 (mk-opcode 'sbc #:cycles 3 #:mode 'zp)
        #xe6 (mk-opcode 'inc #:cycles 5 #:mode 'zp)
        #xe8 (mk-opcode 'inx #:cycles 2 #:mode 'implied)
        #xe9 (mk-opcode 'sbc #:cycles 2 #:mode 'immediate)
        #xea (mk-opcode 'nop #:cycles 2 #:mode 'implied)
        #xec (mk-opcode 'cpx #:cycles 4 #:mode 'absolute)
        #xed (mk-opcode 'sbc #:cycles 4 #:mode 'absolute)
        #xee (mk-opcode 'inc #:cycles 6 #:mode 'absolute)
        #xf0 (mk-opcode 'beq #:cycles 2 #:mode 'relative) ; see bpl
        #xf1 (mk-opcode 'sbc #:cycles 5 #:mode 'indirect #:register 'y)
        #xf5 (mk-opcode 'sbc #:cycles 5 #:mode 'zp #:register 'x)
        #xf6 (mk-opcode 'inc #:cycles 6 #:mode 'zp #:register 'x)
        #xf8 (mk-opcode 'sed #:cycles 2 #:mode 'implied)
        #xf9 (mk-opcode 'sbc #:cycles 4 #:mode 'absolute #:register 'y) ; +1 cycle if page boundary crossed
        #xfd (mk-opcode 'sbc #:cycles 4 #:mode 'absolute #:register 'x) ; +1 cycle if page boundary crossed
        #xfe (mk-opcode 'inc #:cycles 7 #:mode 'absolute #:register 'x)))