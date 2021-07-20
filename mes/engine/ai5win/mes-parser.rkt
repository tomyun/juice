#lang racket/base

(require racket/list)
(require racket/match)
(require racket/port)

(require parsack)

(require "../../mes-config.rkt")

;; parsack

(define-syntax-rule (@ p) (λ (x) (p x)))
(define-syntax-rule (:: p ...) (parser-seq p ...))
(define-syntax-rule (:~ p ...) (parser-one p ...))
(define-syntax-rule (:% p ...) (parser-compose p ...))
(define ($list t p) (>>= p (λ (x) (return (list t x)))))
(define ($cons t p) (>>= p (λ (x) (return (cons t x)))))

(define (char-between a b) (satisfy (λ (c) (char<=? a c b))))
(define (times n p)
  (cond
    [(positive? n) (:% (x <- p)
                       (xs <- (times (sub1 n) p))
                       (return (cons x xs)))]
    [(zero? n) (return '())]))
(define (optional p) (:% (<or> p (return '()))))

;; lexer-util

(define (lex-num1 c) `(num ,(char->integer c)))
(define (lex-num2 l)
  (match-define `(,i1 ,i2) (map char->integer l))
  `(num ,(+ (arithmetic-shift i2 8) i1)))
(define (lex-num4 l)
  (match-define `(,i1 ,i2 ,i3 ,i4) (map char->integer l))
  `(num ,(+ (arithmetic-shift i4 24) (arithmetic-shift i3 16) (arithmetic-shift i2 8) i1)))

(define (lex-num l)
  (define (num x)
    (define (f a) (arithmetic-shift a -2))
    (define (g a b) (bitwise-ior (arithmetic-shift a 8) b))
    (match x
      [`(,a)        (f a)]
      [`(,a ... ,b) (f (g (num a) b))]
      [_            x]))
  `(num ,(num (map char->integer l))))

(define (lex-chr c1 c2)
  (define i (map char->integer `(,c1 ,c2)))
  `(chr-raw ,@i))

(define (lex-lab l)
  ;(define n (integer-bytes->integer (list->bytes (map char->integer l)) #f #f))
  (define n (lex-num4 l))
  `(lab ,n))

;; lexer

(define END    (char #\u00))
(define TXT    (char #\u01))
(define STR    (char #\u02))
(define SETRBC (char #\u03))
(define SETV   (char #\u04))
(define SETRBE (char #\u05))
(define SETAC  (char #\u06)) ;FIXME: how is different from SETAB~?
(define SETA@  (char #\u07))
(define SETAD  (char #\u08))
(define SETAW  (char #\u09))
(define SETAB  (char #\u0A))
(define JNZ    (char #\u0B))
(define JMP    (char #\u0C))
(define SYS    (char #\u0D))
(define GOTO   (char #\u0E))
(define CALL   (char #\u0F))
(define MENUI  (char #\u10))
(define PROC   (char #\u11))
(define UTIL   (char #\u12))
(define LINE   (char #\u13))
(define PROCD  (char #\u14))
(define MENUS  (char #\u15))
(define SETRD  (char #\u16))

(define CHR  (:% (c1 <- (<or> (char-between #\u81 #\u9F)
                              (char-between #\uE0 #\uEF)))
                 (c2 <- (char-between #\u40 #\uFC))
                 (return (lex-chr c1 c2))))
(define chr  CHR)
(define chrs ($cons 'chrs (:: (many chr) (~ END))))

(define str (:% (c <- (many (<or> (char #\u09) ; tab used in ww/CAMP.MES
                                  (char-between #\u20 #\u7E)
                                  (char-between #\uA1 #\uDF))))
                END
                (return (list->string c))))

(define LAB (:% (l <- (times 4 $anyChar)) (return (lex-lab l))))
(define lab LAB)

(define byte  (:% (c <- $anyChar)           (return (lex-num1 c))))
(define word  (:% (l <- (times 2 $anyChar)) (return (lex-num2 l))))
(define dword (:% (l <- (times 4 $anyChar)) (return (lex-num4 l))))

(define NUM1   (:% (c <- (char-between #\u00 #\u7F)) (return (lex-num1 c))))
(define NUM2   (:~ (char #\uF1) (~> word)))
(define NUM4   (:~ (char #\uF2) (~> dword)))
(define NUM    (<or> NUM1 NUM2 NUM4))

(define TERM2  (:% (t <- (<or> (char-between #\uE0 #\uE4)
                               (char-between #\uE6 #\uF0)))
                   (return `(term2 ,t))))
(define TERM1B (:% (t <- (<or> (char #\uA0)
                               (char #\uC0)
                               (char-between #\uF5 #\uF7)))
                   (c <- $anyChar)
                   (return `(term1b ,t ,(lex-num1 c)))))
(define TERM1  (:% (t <- (<or> (char #\uE5)
                               (char #\uF4)))
                   (return `(term1 ,t))))
(define TERM0W (:% (t <- (char #\uF3))
                   (l <- (times 2 $anyChar))
                   (return `(term0w ,t ,(lex-num2 l)))))
;TODO: rename to VAR?
(define TERM0B (:% (t <- (<or> (char #\u80)
                               (char #\uF8)))
                   (c <- $anyChar)
                   (return `(term0b ,t ,(lex-num1 c)))))
(define TERM   (<or> TERM0B TERM0W TERM1 TERM1B TERM2))
(define term  (<or> NUM TERM))

(define VAL    (char #\uFF))
(define expr  ($cons 'expr (manyTill term VAL)))

(define CNT   (char #\u01))
(define exprs ($cons 'exprs (:: (sepBy1 expr CNT) (~ END))))

(define param  ($list 'param (<or> (:~ (char #\u01) (~> str))
                                   (:~ (char #\u02) (~> expr)))))
(define params ($cons 'params (:: (many param) (~ END))))

;; parser

(define op-ret  ($cons 'ret (:: (~ END))))

(define op-txt  ($cons 'txt  (:: (~ TXT) chrs)))
(define op-str  ($cons 'str  (:: (~ STR) str)))
(define op-line ($cons 'line (:: (~ LINE) byte)))

(define op-set  (<or> ($cons 'set-reg:  (:: (~ SETRBC) word exprs))
                      ($cons 'set-reg:: (:: (~ SETRBE) expr exprs))
                      ($cons 'set-reg:d (:: (~ SETRD)  byte exprs))
                      ($cons 'set-var   (:: (~ SETV)   byte exprs))
                      ($cons 'set-arr~@ (:: (~ SETA@)  expr byte exprs))
                      ($cons 'set-arr~  (:: (~ SETAW)  expr byte exprs))
                      ($cons 'set-arr~b (:: (~ SETAB)  expr byte exprs))
                      ($cons 'set-arr~c (:: (~ SETAC)  expr byte exprs))
                      ($cons 'set-arr~d (:: (~ SETAD)  expr byte exprs))))

(define op-jnz  ($cons 'jnz (:: (~ JNZ) expr lab)))
(define op-jmp  ($cons 'jmp (:: (~ JMP) lab)))

(define op-sys  ($cons 'sys  (:: SYS expr params)))
(define op-util ($cons 'util (:: UTIL params)))

(define op-goto ($cons 'goto (:: (~ GOTO) params)))
(define op-call ($cons 'call (:: (~ CALL) params)))

(define op-menu-init ($cons 'menu-init (:: (~ MENUI) params lab)))
(define op-menu-show ($cons 'menu-show (:: (~ MENUS))))

(define op-defproc ($cons 'define-proc (:: (~ PROCD) expr lab)))
(define op-proc    ($cons 'proc (:: (~ PROC) params)))

(define op (<or> op-ret
                 op-txt op-str op-line
                 op-set
                 op-jnz op-jmp
                 op-sys op-util
                 op-goto op-call
                 op-menu-init op-menu-show
                 op-defproc op-proc))

(define stmt  op)
(define stmts (many stmt))

(define <mes> ($cons 'mes (:~ (~> stmts) $eof)))

(define (parser [p <mes>]) p)

(provide parse-result
         parse
         parser)

(provide (prefix-out p: (all-defined-out)))
