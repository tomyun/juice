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
(define (optional p) (:% (<or> p (return '()))))

;; lexer-util

(define (lex-reg0 c) `(num ,(- (char->integer c) #x01)))
(define (lex-reg1 c) (lex-num1 c))
(define (lex-reg2 c1 c2) (lex-num2 c1 c2))

(define (lex-num0 c) `(num ,(- (char->integer c) #x11)))
(define (lex-num1 c) `(num ,(char->integer c)))
(define (lex-num2 c1 c2)
  (match-define `(,i1 ,i2) (map char->integer `(,c1 ,c2)))
  `(num ,(+ (arithmetic-shift i1 8) i2)))

(define (lex-chr c1 c2) `(chr-raw ,@(map char->integer `(,c1 ,c2))))

(define (lex-proc c) `(proc ,(- (char->integer c) #xC0)))

;; lexer

(define REG1 (:% (char #\u00) (c <- $anyChar) (return `(: ,(lex-reg1 c)))))
(define REG0 (:% (c <- (char-between #\u01 #\u07)) (return `(: ,(lex-reg0 c)))))
(define REG2 (:% (char #\u08) (c1 <- $anyChar) (c2 <- $anyChar) (return `(: ,(lex-reg2 c1 c2)))))

(define NUM1 (:% (char #\u10) (c <- $anyChar) (return (lex-num1 c))))
(define NUM0 (:% (c <- (char-between #\u11 #\u17)) (return (lex-num0 c))))
(define NUM2 (:% (char #\u18) (c1 <- $anyChar) (c2 <- $anyChar) (return (lex-num2 c1 c2))))

(define STR (:% (char #\u22)
                (c <- (many (<or> (char-between #\u20 #\u21)
                                  (char-between #\u23 #\u7E)
                                  (char-between #\u80 #\uA0) ;HACK: SJIS code in (str ..) from DK1/BAT.MES
                                  (char-between #\uA1 #\uDF))))
                (char #\u22)
                (return (list->string c))))

(define TERM2 ($list 'term2 (<or> (char #\u21)    ; !=
                                  (char #\u23)    ; ~b
                                  (char #\u25)    ; %
                                  (char #\u26)    ; &&
                                  (char #\u2A)    ; *
                                  (char #\u2B)    ; +
                                  (char #\u2D)    ; -
                                  (char #\u2F)    ; /
                                  (char #\u3C)    ; <
                                  (char #\u3D)    ; ==
                                  (char #\u3E)    ; >
                                  (char #\u5C)    ; ~
                                  (char #\u5E)    ; ^
                                  (char #\u7C)))) ; //
(define TERM0 ($list 'term0 (<or> (char #\u3F)))) ; ?
                            
(define CNT (char #\u2C))

(define VAR ($list 'var (char-between #\u40 #\u5A)))

(define BEG (char #\u7B))
(define END (char #\u7D))

(define CHR (:% (c1 <- (char-between #\u80 #\u98))
                (c2 <- (char-between #\u40 #\uFC))
                (return (lex-chr c1 c2))))

(define CND (char #\u9D)) ;TODO: include in CMD?
(define CMD ($list 'cmd (<or> (char-between #\u99 #\u9C)
                              (char-between #\u9E #\uBF))))

(define PROC (:% (c <- (char-between #\uC0 #\uFF)) (return (lex-proc c))))

;; parser

(define REG   (<or> REG0 REG1 REG2))
(define NUM   (<or> NUM0 NUM1 NUM2))
(define term  (<or> REG NUM VAR TERM0 TERM2))
(define expr  ($cons 'expr (many1 term)))

(define param  ($list 'param (<or> (@ block) STR expr)))
;(define params ($cons 'params (sepBy param CNT)))
(define params ($cons 'params (optional (:% (p <- param)
                                            (l <- (many (try (:~ CNT (~> param)))))
                                            (return `(,p ,@l))))))

(define chr  CHR)
(define chrs ($cons 'chrs (many1 chr)))

(define cut (>> CNT (return `(cut))))

(define op-str  ($list 'str STR))
(define cnd     (:: (~ CND) expr block))
(define op-cnd1 (:% (a <- (try cnd))
                    (b <- (many (try (:~ CNT (~> cnd)))))
                    ;(c <- (optional (:~ CNT (~> block))))
                    (c <- (optional (:~ (optional CNT) (~> (optional block))))) ; missing else in pp1/C1.MES, missing CNT in dk1/FLOOR4.MES
                    (return (cond [(and (empty? b) (empty? c)) `(if ,@a)]
                                  [(empty? b)                  `(if-else ,@a ,c)]
                                  [(empty? c)                  `(cond ,a ,@b)]
                                  [else                        `(cond ,a ,@b (else ,c))]))))
(define op-cnd2 ($cons 'if (:: (~ CND) expr block*)))
(define op-cnd  (<or> op-cnd1 op-cnd2))
(define op-cmd  (:: CMD params))
(define op-proc PROC)
(define op      (<or> op-str op-cnd op-cmd op-proc))

(define block  ($cons '<> (:~ BEG (~> stmts) END)))
(define block* ($cons '<*> (many (<or> op chrs)))) ; many instead of many1 for dr3/SHOP.MES
(define stmt  (<or> block cut op expr chrs)) ;TODO: check use of expr in set-mem for dk2/OPEN.MES
(define stmts (many stmt))

(define <mes> ($cons 'mes (:~ (~> stmts) END (optional $eof)))) ; garbage before EOF in raygun/FL2-5|6.MES

(define (parser [p <mes>]) p)

(provide parse-result
         parse
         parser)

(provide (prefix-out p: (all-defined-out)))
