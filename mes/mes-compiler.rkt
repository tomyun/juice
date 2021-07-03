#lang racket/base

(require (for-syntax racket/base))
(require racket/file)
(require racket/format)
(require racket/function)
(require racket/include)
(require racket/list)
(require racket/match)
(require syntax/parse (for-syntax syntax/parse))
(require racket/provide)
(require racket/string (for-syntax racket/string))

(require "mes-config.rkt")
(require "mes-charset.rkt")

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

(define (mes:sys i [n #f]) (λ p `(,(mes:sys* i n) ,@(mes:params p))))
(define (mes:sys* i [n #f])
  (if n `(,SYS ,(integer->char i) ,(num n 3))
        `(,SYS ,(integer->char i))))

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

;;HACK: counterpart for replacing enhanced (text ...) in translated scripts
(define (mes:str #:color [c #f] . l)
  (define k (if c `(,(mes:text-color c)) '()))
  (define (f t)
    (match t
      [(? string? s) (mes:str* s)]
      [(? number? n) (mes:proc n)]   ; 0: nanpa1 & etc, 3: kakyu
      [(? char?   c) (mes:call c)])) ; Z: elle
  `(,@k ,@(map f l)))
(define (mes:str* s) `(,STR ,@(string->list s) ,STR))

(define (mes:num n)
  (cond
   [(< n 0)         (error (format "negative number not supported: ~a" n))]
   [(<= n #x0F)     (integer->char (+ n (char->integer NUM0)))] ; 15
   [(<= n #x3F)     `(,NUM1 ,@(num n))] ; 63
   [(<= n #x0FFF)   `(,NUM2 ,@(num n))] ; 4095
   [(<= n #x03FFFF) `(,NUM3 ,@(num n))] ; 262143
   [else            (error (format "too large number: ~a" n))]))

(define (num n [i 0])
  (define (: r x) `(,(bitwise-ior (arithmetic-shift (bitwise-and x #x3F) 2) #x03) ,@r))
  (define (:: x)  (arithmetic-shift x -6))
  (define (f l)
    (match l
      [`(() 0)  (: '() 0)]
      [`(,r 0)  r]
      [`(,r ,x) (f `(,(: r x) ,(:: x)))]))
  (define v (f `(() ,n)))
  (define p (max 0 (- i (length v))))
  (define v1 ((apply compose1 (make-list p (λ (r) (: r 0)))) v))
  (map integer->char v1))

(define (mes:set-reg:  v   . e)
  (match v
   [(? number? n)               `(,SETRC    ,(mes:num n)   ,@(mes:exprs e))]
   [(? mes:var? x)              `(,SETRE    ,(mes:expr x)  ,@(mes:exprs e))]
   [`(,l ...)                   `(,SETRE    ,(mes:expr l)  ,@(mes:exprs e))]))
(define (mes:set-reg:: v   . e) `(,SETRE    ,(mes:expr v)  ,@(mes:exprs e)))
(define (mes:set-var   v e)     `(,SETV  ,v ,(mes:expr e)))
(define (mes:set-arr~  v i . e) `(,SETAW ,v ,(mes:expr i)  ,@(mes:exprs e)))
(define (mes:set-arr~b v i . e) `(,SETAB ,v ,(mes:expr i)  ,@(mes:exprs e)))

(define (mes:if      c t)   `(,CND ,(mes:expr c) ,t))
(define (mes:if-else c t e) `(,CND ,(mes:expr c) ,t ,CNT ,e))
(define-syntax mes:cond
  (syntax-rules (else)
    [(_ [else e])       `(,e)]
    [(_ [c    t])       `(,(mes:if c t))]
    [(_ [c    t] l ...) `(,(mes:if c t) ,CNT ,(mes:cond l ...))]))

(define (mes:cmd i) (λ p `(,(integer->char i) ,@(mes:params p))))

(define mes:text-color  (mes:cmd #x10))
(define mes:wait        (mes:cmd #x11))
(define mes:define-proc (mes:cmd #x12))
(define mes:proc        (mes:cmd #x13))
(define mes:call        (mes:cmd #x14))
(define mes:number      (mes:cmd #x15)) 
(define mes:delay       (mes:cmd #x16))
(define mes:clear       (mes:cmd #x17))
(define mes:color       (mes:cmd #x18))
(define mes:util        (mes:cmd #x19))
(define mes:animate     (mes:cmd #x1A))

;TODO: macro implementation
(define mes:@ #\@)
(define mes:A #\A)
(define mes:B #\B)
(define mes:C #\C)
(define mes:D #\D)
(define mes:E #\E)
(define mes:F #\F)
(define mes:G #\G)
(define mes:H #\H)
(define mes:I #\I)
(define mes:J #\J)
(define mes:K #\K)
(define mes:L #\L)
(define mes:M #\M)
(define mes:N #\N)
(define mes:O #\O)
(define mes:P #\P)
(define mes:Q #\Q)
(define mes:R #\R)
(define mes:S #\S)
(define mes:T #\T)
(define mes:U #\U)
(define mes:V #\V)
(define mes:W #\W)
(define mes:X #\X)
(define mes:Y #\Y)
(define mes:Z #\Z)

(define (mes:var c) c)
(define (mes:var? v)
 (match v
  [(? char? c) (char<=? #\u40 c #\u5A)]
  [_           #f]))

(define (mes:chr-raw c1 c2)
  (define c0 (hash-ref dict `(,c1 ,c2) #f))
  (map integer->char
       (if (and (cfg:compress) c0)
           `(,(+ c0 (cfg:dictbase)))
           `(,(- c1 #x20) ,c2))))

(define (text-wrap l w)
  (define (: l)
    (match l
      [`(,(? string? s) ,r ...) `(,@(text-wrap* s w) ,@(: r))]
      [`(,a             ,r ...) `(,a                 ,@(: r))]
      [a                        a]))
  (: l))

(define (text-wrap* s w)
  (define (measure s) (* (string-length s) (cfg:fontwidth)))
  (define (chop l w)
    (let-values ([(i c wl rl)
                  (for/fold ([i  1]
                             [c  0]
                             [wl '()]
                             [rl l])
                            ([s l]
                             [n (map (compose1 add1 measure) l)]
                             #:break (< w (+ c n)))
                    (values (add1 i) (+ c n) `(,@wl ,s) (drop rl 1)))])
      `(,wl ,rl)))
  (define (: l w)
    (match l
      [`(,s ... ())       s]
      [`(,s ... (,r ...)) (: `(,@s ,@(chop r w)) w)]))
  (define spc (cfg:charspc))
  (define (join s) (string-join s (string spc)))
  (define (fill s w)
    (define n (measure s))
    (define f (make-string (max (- w n) 0) spc))
    (string-append s f))
  (if w
    (let* ([l (string-split s)]
           [n (apply max (map measure l))])
      (if (> w (add1 n))
        (let ([fs (map join (: `(,l) (* (quotient w 2) 2)))])
          `(,@(map (curryr fill w) (drop-right fs 1)) ,(last fs)))
        `(,s)))
    `(,s)))

(define (mes:text #:color [color #f]
                  #:wrap  [wrap  #t]
                  . l)
  (define c (if color `(,(mes:text-color color)) '()))
  (define w (cond
              [(number? wrap) wrap]
              [wrap           (cfg:wordwrap)]
              [(not wrap)     #f]))
  (define (: t)
    (match t
      [(? string? s) (mes:text* s)]
      [(? symbol? s) (mes:text-func s)]
      [(? number? n) (mes:proc n)]   ; 0: nanpa1 & etc, 3: kakyu
      [(? char?   c) (mes:call c)])) ; Z: elle
  `(,@c ,@(map : (text-wrap l w))))

(define (mes:text-func s)
  (match s
    [(or 'n 'newline) (mes:text-newline)]))

(define (mes:text-newline)
  (mes:set-arr~ #\@ 17 (mes:~ #\@ 13)
                       (mes:+ (mes:~ #\@ 18) (mes:&& (mes:~ #\@ 21) #xFF))))

(define (mes:text* s)
  (define l (flatten (map char->sjis (string->list s))))
  (define (f l)
    (match l
     [`(,c1 ,c2 ,r ...) `(,@(mes:chr-raw c1 c2) ,@(f r))]
     [x                 x]))
  (f l))

(define (mes:text-raw . l)
  (define (f i)
    (match-define `(,c1 ,c2) (integer->sjis i))
    (mes:chr-raw c1 c2))
  (flatten (map f l)))

(define (mes:dict . l)
  (define (f d)
    (match d
     [(? char? c) (char->sjis c)]
     [`(,c1 ,c2)  `(,c1 ,c2)]))
  (define K (map f l))
  (apply mes:dict* K))

(define (mes:dict* . K)
  (define n (length K))
  (define V (range n))
  (set! dict (make-hash (map cons K V)))
  (define (bytes->chars b) (map integer->char (bytes->list b)))
  (define s (bytes->chars (integer->integer-bytes (* (add1 n) 2) 2 #f #f)))
  (define D (map integer->char (flatten K)))
  `(,s ,D))

(define (mes:dict-build)
  (define h (make-hash))
  (define (fc j)
    (cond
      [(hash-has-key? h j) (hash-update! h j (λ (i) (add1 i)))]
      [else                (hash-set!    h j 1)]))
  (define (ft l)
    (match l
      [(? string? s) (map (compose1 fc char->sjis) (string->list s))]
      [a             null]))
  (define (: l)
    (match l
      [`(mes ,a ,r ...)            `(,(: a)        ,@(: r))]
      [`((text    ,t ...)  ,r ...) `(,@(map ft t)  ,@(: r))]
      [`((chr-raw ,c1 ,c2) ,r ...) `(,(fc (c1 c2)) ,@(: r))]
      [`(,a                ,r ...) `(,(: a)        ,@(: r))]
      [a                           a]))
  (: src)
  (define n (add1 (- #xFF (cfg:dictbase))))
  (define K
    (let* ([l (hash->list h)]
           [l (filter (λ (x) (> (cdr x) 1)) l)]
           [l (sort l (λ (x y) (> (cdr x) (cdr y))))]
           [l (take l (min n (length l)))]
           [l (map car l)])
           ;[l (sort l (λ (x y) (< (sjis->integer x) (sjis->integer y))))])
      l))
  (apply mes:dict* K))

(define (mes:cut) `(,CNT))
 
(define (mes:expr e) `(,(mes:term e) ,VAL))
(define (mes:exprs . e)
  (define (f x)
    (match x
      [`(() (,r ... ,a))       (f `((,(mes:expr a))          ,r))]
      [`((,l ...) (,r ... ,a)) (f `((,(mes:expr a) ,CNT ,@l) ,r))]
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

;;TODO: use macro to reduce repetition
(define (mes:+  . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u20)]
   [`(,r ... ,a)        (mes:+ (apply mes:+ r) a)]))
(define (mes:-  . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u21)]
   [`(,r ... ,a)        (mes:- (apply mes:- r) a)]))
(define (mes:*  . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u22)]
   [`(,r ... ,a)        (mes:* (apply mes:* r) a)]))
(define (mes:/  . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u23)]
   [`(,r ... ,a)        (mes:/ (apply mes:/ r) a)]))
(define (mes:%     a b) (mes:term2 a b #\u24))
(define (mes:// . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u25)]
   [`(,r ... ,a)        (mes:// (apply mes:// r) a)]))
(define (mes:&& . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u26)]
   [`(,r ... ,a)        (mes:&& (apply mes:&& r) a)]))
(define (mes:==    a b) (mes:term2 a b #\u27))
(define (mes:!=    a b) (mes:term2 a b #\u28))
(define (mes:>     a b) (mes:term2 a b #\u29))
(define (mes:<     a b) (mes:term2 a b #\u2A))
(define (mes:~     a b) (mes:term2 a b #\u2B))
(define (mes:~b    a b) (mes:term2 a b #\u2C))
(define (mes::     a)
  (match a
    [(? number? n)      (mes:term0 n #\u2D)]
    [a                  (mes:term1 a #\u2E)]))
(define (mes:::    a)   (mes:term1 a #\u2E))
(define (mes:?     a)   (mes:term0 a #\u2F))

(define (mes:_) '()) ; empty expr, pointing last stack value

(define (mes:param p)
  (match p
   [(? mes:block? b)    b]
   [(? string? s)       (mes:str s)]
   [e                   (mes:expr e)]))
(define (mes:params . p)
  (define (f x)
    (match x
      [`(()       (,r ... ,a)) (f `((,(mes:param a))          ,r))]
      [`((,l ...) (,r ... ,a)) (f `((,(mes:param a) ,CNT ,@l) ,r))]
      [`((,l ...) ())          l]))
  (f `(() ,@p)))

(define (mes:<>  . l) `(,BEG ,@l ,END))
(define (mes:<>* . l) `(,@l))

(define (mes:block? b)
  (match b
   [`(,(? char? a) ,l ... ,(? char? b)) (and (char=? a BEG) (char=? b END))]
   [_                                   #f]))
(define (mes:mes  . l) (flatten `(,l ,END)))
(define (mes:mes* . l) (flatten l))

;; state

(define src '())
(define dict (hash))

(define (mes:init l)
  (set! src l)
  (set! dict (hash))
  (charset (cfg:charset)))

;; meta

(define (mes:meta . l) '())
(define (mes:charset f) (charset f))
(define (mes:charset*  k t . l) (apply charset* k t l))
(define (mes:charset** k t . l) (apply charset** k t l))
(define (mes:charspc c) (cfg:charspc c))
(define (mes:fontwidth w) (cfg:fontwidth w))
(define (mes:dictbase b) (cfg:dictbase b))
(define (mes:extraop x) (cfg:extraop x))
(define (mes:wordwrap w) (cfg:wordwrap w))

;; extension

(define (mes:include f) (eval `(mes* ,@(file->list f))))

;; compiler-util

(define (char->hex c) (~r (char->integer c) #:base 16 #:min-width 2 #:pad-string "0"))
(define (show-hex l) (string-join (map char->hex (flatten l))))

(provide show-hex)

;; compatibility
(include "mes-compiler-compat.rkt")

(provide (filtered-out
          (λ (n) (and (string-prefix? n "mes:") (string-replace n "mes:" "")))
          (all-defined-out)))
