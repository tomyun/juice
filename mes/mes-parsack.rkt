#lang racket/base

(require racket/list)
(require racket/match)
(require racket/port)

(require parsack)

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

(define (lex-num0 c) (- (char->integer c) #x30))

(define (lex-num l)
  (define (num x)
    (define (f a) (arithmetic-shift a -2))
    (define (g a b) (bitwise-ior (arithmetic-shift a 8) b))
    (match x
      [`(,a)        (f a)]
      [`(,a ... ,b) (f (g (num a) b))]
      [_            x]))
  (num (map char->integer l)))

(define (lex-chr c1 c2)
  (define i (map char->integer `(,c1 ,c2)))
  (define m (match i [`(,a ,b) `(,(+ a #x20) ,b)]))
  (define b (list->bytes m))
  ;(bytes->string/latin-1 b))
  ;(apply string (map integer->char m)))
  (read-char (reencode-input-port (open-input-bytes b) "sjis")))

(define (lex-dic c) (- (char->integer c) #x80))

;; lexer

(define END   (char #\u00))
(define BEG   (char #\u01))
(define CNT   (char #\u02))
(define VAL   (char #\u03))
(define SYS   (:% (char #\u04)
                  (c <- (char-between #\u10 #\uFF))
                  (return `(sys ,c))))
(define STR   (:% (char #\u06)
                  (c <- (many (<or> (char-between #\u20 #\u7E)
                                    (char-between #\uA1 #\uDF))))
                  (char #\u06)
                  (return (list->string c))))
(define NUM1  (:% (char #\u07) (c <- $anyChar)           (return `(num ,(lex-num `(,c))))))
(define NUM2  (:% (char #\u08) (c <- (times 2 $anyChar)) (return `(num ,(lex-num c)))))
(define NUM3  (:% (char #\u09) (c <- (times 3 $anyChar)) (return `(num ,(lex-num c)))))
(define SETRC (char #\u0A))
(define SETRE (char #\u0B))
(define SETV  (char #\u0C))
(define SETAW (char #\u0D))
(define SETAB (char #\u0E))
(define CND   (char #\u0F))
(define CMD   ($list 'cmd (char-between #\u10 #\u1F)))
(define TERM2 ($list 'term2 (char-between #\u20 #\u2C)))
(define TERM1 ($list 'term1 (char #\u2E)))
(define TERM0 ($list 'term0 (<or> (char #\u2D) (char #\u2F))))
(define NUM0  (:% (c <- (char-between #\u30 #\u3F)) (return `(num ,(lex-num0 c)))))
(define VAR   ($list 'var (char-between #\u40 #\u5A)))
(define CHR   (:% (c1 <- (char-between #\u60 #\u7F))
                  (c2 <- (char-between #\u40 #\uFC))
                  (return `(chr ,(lex-chr c1 c2)))))
(define DIC   (:% (c <- (char-between #\u80 #\uFF)) (return `(dic ,(lex-dic c)))))

;; parser

(define NUM   (<or> NUM0 NUM1 NUM2 NUM3))
(define term  (<or> NUM VAR TERM0 TERM1 TERM2))
(define expr  ($cons 'expr (manyTill term VAL)))
(define exprs ($cons 'exprs (sepBy1 expr CNT)))

(define param  ($list 'param (<or> (@ block) STR expr)))
;(define params ($cons 'params (sepBy param CNT)))
(define params ($cons 'params (optional (:% (p <- param)
                                            (l <- (many (try (:~ CNT (~> param)))))
                                            (return `(,p ,@l))))))

(define chr  (<or> CHR DIC))
(define chrs ($cons 'chrs (many1 chr)))
;(define texts ($cons 'texts (sepBy1 chrs CNT)))

(define cut (>> CNT (return `(cut))))

(define op-sys  (:: SYS params))
(define op-str  ($list 'str STR))
(define op-set  (<or> ($cons 'set-reg   (:: (~ SETRC) NUM exprs))
                      ($cons 'set-reg*  (:: (~ SETRE) expr exprs))
                      ($cons 'set-var   (:: (~ SETV)  VAR expr))
                      ($cons 'set-arr   (:: (~ SETAW) VAR expr exprs))
                      ($cons 'set-arr.b (:: (~ SETAB) VAR expr exprs))))
(define cnd     (:: (~ CND) expr block))
(define op-cnd1 (:% (a <- (try cnd))
                    (b <- (many (try (:~ CNT (~> cnd)))))
                    ;(c <- (optional (:~ CNT (~> block))))
                    (c <- (optional (:~ CNT (~> (optional block))))) ; missing else in np2/TOWN1.MES
                    (return (cond [(and (empty? b) (empty? c)) `(if ,@a)]
                                  [(empty? b)                  `(if-else ,@a ,c)]
                                  [(empty? c)                  `(cond ,a ,@b)]
                                  [else                        `(cond ,a ,@b (else ,c))]))))
(define op-cnd2 ($cons 'if (:: (~ CND) expr chrs)))
(define op-cnd  (<or> op-cnd1 op-cnd2))
(define op-cmd  (:: CMD params))
(define op      (<or> op-sys op-str op-set op-cnd op-cmd))

(define block ($cons 'begin (:~ BEG (~> stmts) END)))
(define stmt  (<or> block cut op chrs))
(define stmts (many stmt))

(define <mes> ($cons 'mes (:~ (~> stmts) (optional END) (optional $eof)))) ; many inconsistent endings

(provide parse-result
         parse
         <mes>)

(provide (prefix-out p: (all-defined-out)))
