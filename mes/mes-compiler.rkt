#lang racket/base

(require (for-syntax racket/base))
(require racket/format)
(require racket/list)
(require racket/match)
(require syntax/parse (for-syntax syntax/parse))
(require racket/provide)
(require racket/string (for-syntax racket/string))

;; lexer

(define END   #\u00)
(define BEG   #\u01)
(define CNT   #\u02)
(define VAL   #\u03)
(define SYS   #\u04)
(define STR   #\u06)
(define NUM1  #\u07)
(define NUM2  #\u08)
(define NUM3  #\u09)
(define SETRC #\u0A)
(define SETRE #\u0B)
(define SETV  #\u0C)
(define SETAW #\u0D)
(define SETAB #\u0E)
(define CND   #\u0F)
(define NUM0  #\u30)

;; compiler

(define (mes:sys i) (λ p `(,(mes:sys* i) ,@(mes:params p))))
(define (mes:sys* i) `(,SYS ,(integer->char i)))

(define (mes:while c t) `(,(mes:sys* #x10) ,(mes:if c t)))
(define mes:continue    (mes:sys #x11))
(define mes:break       (mes:sys #x12))
(define mes:menu-show   (mes:sys #x13))
(define mes:menu-init   (mes:sys #x14))
(define mes:mouse       (mes:sys #x15))
(define mes:palette     (mes:sys #x16))
(define mes:box         (mes:sys #x17))
(define mes:box-inv     (mes:sys #x18))
(define mes:blit        (mes:sys #x19))
(define mes:blit-swap   (mes:sys #x1A))
(define mes:blit-mask   (mes:sys #x1B))
(define mes:load-file   (mes:sys #x1C))
(define mes:load-image  (mes:sys #x1D))
(define mes:mes-jump    (mes:sys #x1E))
(define mes:mes-call    (mes:sys #x1F))
(define mes:flag        (mes:sys #x21))
(define mes:slot        (mes:sys #x22))
(define mes:click       (mes:sys #x23))
(define mes:sound       (mes:sys #x24))
(define mes:field       (mes:sys #x26))

(define (mes:str s) `(,STR ,@(string->list s) ,STR))

(define (mes:num n)
  (define (f l)
    (match l
      [`(() 0)  0]
      [`(,r 0)  r]
      [`(,r ,x) (f `(,(cons (bitwise-ior (arithmetic-shift (bitwise-and x #x3F) 2) #x03) r)
                     ,(arithmetic-shift x -6)))]))
  (define (g n) (map integer->char (f `(() ,n))))
  (cond
   [(< n 0)         (error (format "negative number not supported:e ~a" n))]
   [(<= n #x0F)     (integer->char (+ n (char->integer NUM0)))] ; 15
   [(<= n #x3F)     (cons NUM1 (g n))] ; 63
   [(<= n #x0FFF)   (cons NUM2 (g n))] ; 4095
   [(<= n #x03FFFF) (cons NUM3 (g n))] ; 262143
   [else            (error (format "too large number:e ~a" n))]))

(define (mes:set-reg v . e)
  (match v
   [(? number? n)               `(,SETRC    ,(mes:num n)   ,@(mes:exprs e))]
   [(? mes:var? x)              `(,SETRE    ,(mes:expr x)  ,@(mes:exprs e))]
   [`(,l ...)                   `(,SETRE    ,(mes:expr l)  ,@(mes:exprs e))]))
(define (mes:set-reg* v . e)    `(,SETRE    ,(mes:expr v)  ,@(mes:exprs e)))
(define (mes:set-var v e)       `(,SETV  ,v ,(mes:expr e)))
(define (mes:set-arr v i . e)   `(,SETAW ,v ,(mes:expr i)  ,@(mes:exprs e)))
(define (mes:set-arr.b v i . e) `(,SETAB ,v ,(mes:expr i)  ,@(mes:exprs e)))

(define (mes:if c t)        `(,CND ,(mes:expr c) ,t))
(define (mes:if-else c t e) `(,CND ,(mes:expr c) ,t ,CNT ,e))
(define-syntax mes:cond
  (syntax-rules (else)
    [(_ [else e])    `(,e)]
    [(_ [c t])       `(,(mes:if c t))]
    [(_ [c t] l ...) `(,(mes:if c t) ,CNT ,(mes:cond l ...))]))

(define (mes:cmd i) (λ p `(,(integer->char i) ,@(mes:params p))))

(define mes:text-color   (mes:cmd #x10))
(define mes:wait         (mes:cmd #x11))
(define mes:define-proc  (mes:cmd #x12))
(define mes:proc         (mes:cmd #x13))
(define mes:call         (mes:cmd #x14))
(define mes:print-number (mes:cmd #x15)) 
(define mes:delay        (mes:cmd #x16))
(define mes:clear        (mes:cmd #x17))
(define mes:color        (mes:cmd #x18))
(define mes:util         (mes:cmd #x19))
(define mes:animate      (mes:cmd #x1A))

(define (mes:var c) c)
(define (mes:var? v)
 (match v
  [(? char? c) (char<=? #\u40 c #\u5A)]
  [_           #f]))

(define dict (hash))

(define (mes:chr-raw c1 c2)
  (define c0 (hash-ref dict `(,c1 ,c2) #f))
  (map integer->char
       (if c0
           `(,(+ c0 #x80))
           `(,(- c1 #x20) ,c2))))

(define tbl (make-hash))

(define (sjis k t)
  (define s1 (cond [(<=  1 k 62) (floor (/ (+ k 257) 2))]
                   [(<= 63 k 94) (floor (/ (+ k 385) 2))]))
  (define s2 (cond [(even? k)    (+ t 158)]
                   [(<=  1 t 63) (+ t 63)]
                   [(<= 64 t 94) (+ t 64)]))
  `(,s1 ,s2))

(define (mes:tbl k t . l)
  (for ([c l]
        [i (range (length l))])
    (define j (sjis (+ k (quotient (+ (sub1 t) i) 94))
                    (add1 (remainder (+ (sub1 t) i) 94))))
    (hash-set! tbl c j))
  '())

(define (char->sjis c)
  (define b8 (string->bytes/utf-8 (string c)))
  (define t (bytes-open-converter "utf-8" "shift_jisx0213"))
  ;(define-values (b n r) (bytes-convert t (string->bytes/utf-8 s)))
  ;FIXME: avoid iconv issue: https://github.com/racket/racket/issues/3876
  (define b (let-values ([(b n r) (bytes-convert t b8)])
              (if (zero? (bytes-length b))
                  (let-values ([(b n r) (bytes-convert t b8)]) b)
                  b)))
  (bytes-close-converter t)
  (bytes->list b))

(define (mes:text s)
  (define (: c)
    (define c1 (hash-ref tbl c #f))
    (if c1 c1 (char->sjis c)))
  (define l (flatten (map : (string->list s))))
  (define (f l)
    (match l
     [`(,c1 ,c2 ,r ...) `(,@(mes:chr-raw c1 c2) ,@(f r))]
     [x                 x]))
  (f l))

(define (mes:text-raw s)
  (define (f c)
    (define i (char->integer c))
    (define c1 (arithmetic-shift (bitwise-and i #xFF00) -8))
    (define c2 (bitwise-and i #x00FF))
    (map integer->char `(,(- c1 #x20) ,c2)))
  (flatten (map f (string->list s))))

(define (mes:dict . l)
  (define n (length l))
  (define K (map char->sjis l))
  (define V (range n))
  (set! dict (make-hash (map cons K V)))
  (define (bytes->chars b) (map integer->char (bytes->list b)))
  (define s (bytes->chars (integer->integer-bytes (* (add1 n) 2) 2 #f #f)))
  (define D (map integer->char (flatten K)))
  `(,s ,D))

(define (mes:cut) `(,CNT))
 
(define (mes:expr e) `(,(mes:term e) ,VAL))
(define (mes:exprs . e)
  (define (f x)
    (match x
      [`(() (,r ... ,a))       (f `((,(mes:expr a)) ,r))]
      [`((,l ...) (,r ... ,a)) (f `(,(append `(,(mes:expr a) ,CNT) l) ,r))]
      [`((,l ...) ())          l]))
  (f `(() ,@e)))

(define (mes:term x) ; HACK: handle num, var, expr
  (match x
    [(? number? n) (mes:num n)]
    [(? string? s) (mes:str s)]
    [x             x]))
(define (mes:term2 a b c) `(,(mes:term a) ,(mes:term b) ,c))
(define (mes:term1 a c)   `(,(mes:term a) ,c))
(define (mes:term0 a c)   `(,c ,(mes:term a)))

(define (mes:+     a b) (mes:term2 a b #\u20))
(define (mes:-     a b) (mes:term2 a b #\u21))
(define (mes:*     a b) (mes:term2 a b #\u22))
(define (mes:/     a b) (mes:term2 a b #\u23))
(define (mes:%     a b) (mes:term2 a b #\u24))
(define (mes:! . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u25)]
   [`(,r ... ,a)        (mes:! (apply mes:! r) a)]))
(define (mes:& . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u26)]
   [`(,r ... ,a)        (mes:& (apply mes:& r) a)]))
(define (mes:==    a b) (mes:term2 a b #\u27))
(define (mes:!=    a b) (mes:term2 a b #\u28))
(define (mes:>     a b) (mes:term2 a b #\u29))
(define (mes:<     a b) (mes:term2 a b #\u2A))
(define (mes:arr   a b) (mes:term2 a b #\u2B))
(define (mes:arr.b a b) (mes:term2 a b #\u2C))
(define (mes:reg   a)
  (match a
    [(? number? n)      (mes:term0 n #\u2D)]
    [a                  (mes:term1 a #\u2E)]))
(define (mes:rnd a)     (mes:term0 a #\u2F))

(define (mes:_) '()) ; empty expr, pointing last stack value

(define (mes:param p)
  (match p
   [(? mes:block? b)    b]
   [(? string? s)       (mes:str s)]
   [e                   (mes:expr e)]))
(define (mes:params . p)
  (define (f x)
    (match x
      [`(() (,r ... ,a))       (f `((,(mes:param a)) ,r))]
      [`((,l ...) (,r ... ,a)) (f `(,(append `(,(mes:param a) ,CNT) l) ,r))]
      [`((,l ...) ())          l]))
  (f `(() ,@p)))

(define (mes:begin . l)  `(,BEG ,@l ,END))
(define (mes:begin* . l) `(,@l))
(define (mes:block? b)
  (match b
   [`(,(? char? a) ,l ... ,(? char? b)) (and (char=? a BEG) (char=? b END))]
   [_                                   #f]))
(define (mes:mes . l) (flatten `(,l ,END)))

(define (mes:init)
  (set! dict (hash))
  (set! tbl (make-hash)))

;; compiler-util

(define (char->hex c) (~r (char->integer c) #:base 16 #:min-width 2 #:pad-string "0"))
(define (show-hex l) (string-join (map char->hex (flatten l))))

(provide show-hex)

(provide (filtered-out
          (λ (n) (and (string-prefix? n "mes:") (string-replace n "mes:" "")))
          (all-defined-out)))
