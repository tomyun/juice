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

(require "../../mes-config.rkt")
(require "../../mes-charset.rkt")

;; lexer

(define REG1 #\u00)
(define REG0 #\u01)
(define REG2 #\u08)
(define NUM1 #\u10)
(define NUM0 #\u11)
(define NUM2 #\u18)
(define STR  #\u22)
(define CNT  #\u2C)
(define BEG  #\u7B)
(define END  #\u7D)
(define CND  #\u9D)
(define PROC #\uC0)

;; compiler

(define (mes:reg n)
  (cond
    [(< n 0)       (error (format "negative number not supported: ~a" n))]
    [(<= n 6)      (integer->char (+ n (char->integer REG0)))]
    [(<= n #xFF)   `(,REG1 ,(integer->char n))]
    [(<= n #xFFFF) `(,REG2 ,@(map integer->char `(,(arithmetic-shift (bitwise-and n #xFF00) -8) ,(bitwise-and n #xFF))))]
    [else          (error (format "too large number: ~a" n))]))

(define (mes:num n)
  (cond
    [(< n 0)       (error (format "negative number not supported: ~a" n))]
    [(<= n 6)      (integer->char (+ n (char->integer NUM0)))]
    [(<= n #xFF)   `(,NUM1 ,(integer->char n))]
    [(<= n #xFFFF) `(,NUM2 ,@(map integer->char `(,(arithmetic-shift (bitwise-and n #xFF00) -8) ,(bitwise-and n #xFF))))]
    [else          (error (format "too large number: ~a" n))]))

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

(define (mes:expr e) (mes:term e))
(define (mes:term x) ; HACK: handle num, var, expr
  (match x
    [(? number? n) (mes:num n)]
    ;[(? string? s) (mes:str s)]
    [x             x]))
(define (mes:term2 a b c) `(,(mes:term a) ,(mes:term b) ,c))
(define (mes:term0 a c)   `(,c ,(mes:term a)))

;;TODO: use macro to reduce repetition
(define (mes:+  . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u2B)]
   [`(,r ... ,a)        (mes:+ (apply mes:+ r) a)]))
(define (mes:-  . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u2D)]
   [`(,r ... ,a)        (mes:- (apply mes:- r) a)]))
(define (mes:*  . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u2A)]
   [`(,r ... ,a)        (mes:* (apply mes:* r) a)]))
(define (mes:/  . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u2F)]
   [`(,r ... ,a)        (mes:/ (apply mes:/ r) a)]))
(define (mes:%     a b) (mes:term2 a b #\u25))
(define (mes:^     a b) (mes:term2 a b #\u5E))
(define (mes:// . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u7C)]
   [`(,r ... ,a)        (mes:// (apply mes:// r) a)]))
(define (mes:&& . l)
  (match l
   [`(,a ,b)            (mes:term2 a b #\u26)]
   [`(,r ... ,a)        (mes:&& (apply mes:&& r) a)]))
(define (mes:==    a b) (mes:term2 a b #\u3D))
(define (mes:!=    a b) (mes:term2 a b #\u21))
(define (mes:>     a b) (mes:term2 a b #\u3E))
(define (mes:<     a b) (mes:term2 a b #\u3C))
(define (mes:~     a b) (mes:term2 a b #\u5C))
(define (mes:~b    a b) (mes:term2 a b #\u23))
(define (mes::     a)   (mes:reg a))
(define (mes:?     a)   (mes:term0 a #\u3F))

(define (mes:_) '()) ; empty expr, pointing last stack value

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
  (map integer->char `(,c1 ,c2)))

(define (text-wrap l w)
  (define (rearrange l)
    (define (slice s)
      (define r (regexp-match-positions* #rx"\n+|$" s))
      (define n (remove-duplicates (map cdr r)))
      (for/list ([i `(0 ,@(drop-right n 1))]
                 [j n])
        (substring s i j)))
    (define (:/ x)
      (match x
        [`(,(? string? s) ,r ...) `(,@(slice s) ,@(:/ r))]
        [`(,a ,r ...)             `(,(:/ a) ,@(:/ r))]
        [x                        x]))
    (define (sameline? s) (not (string-suffix? s "\n")))
    (define (:* x)
      (match x
        [`(,(? string? s1) ,(? string? s2) ,r ...) #:when (sameline? s1) (:* `(,(string-append s1 s2) ,@r))]
        [`(,a ,r ...)                                                    `(,(:* a) ,@(:* r))]
        [x                                                               x]))
    (:* (:/ l)))
  (define (: l)
    (match l
      [`(,(? string? s) ,r ...) `(,@(text-wrap* s w) ,@(: r))]
      [`(,a             ,r ...) `(,a                 ,@(: r))]
      [a                        a]))
  (: (rearrange l)))

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
  (define spc (~a (cfg:char-space)))
  (define (split s) (string-split s spc #:trim? #f))
  (define (join s) (string-join s spc))
  (define (wrap s)
    (if (string-suffix? s "\n")
      s
      (string-append s (string (cfg:char-newline)))))
  (if w
    (let* ([l (split s)]
           [n (apply max (map measure l))])
      (if (> w (add1 n))
        (let ([fs (map join (: `(,l) (* (quotient w 2) 2)))])
          `(,@(map wrap (drop-right fs 1)) ,(last fs)))
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
      [(? char?   c) (mes:call c)]   ; Z: elle
      [t             t]))
  `(,@c ,@(map : (text-wrap l w))))

;; nothing implemented at this moment
(define (mes:text-func s)
  (match s
    [_ '()]))

(define (mes:text* s)
  (define l
    (with-handlers ([exn:fail? (λ (v) (error (format "(text ~v) got ~a" s (exn-message v))))])
      (flatten (map char->sjis (string->list s)))))
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

(define (mes:cmd i) (λ p `(,(mes:cmd* i) ,@(mes:params p))))
(define (mes:cmd* i) `(,(integer->char i)))

(define mes:set-reg:        (mes:cmd #x99))
(define mes:set-var         (mes:cmd #x9A))
(define mes:set-arr~        (mes:cmd #x9B))
(define mes:set-arr~b       (mes:cmd #x9C))

(define (mes:if      c t)   `(,CND ,(mes:expr c) ,t))
(define (mes:if-else c t e) `(,CND ,(mes:expr c) ,t ,CNT ,e))
(define-syntax mes:cond
  (syntax-rules (else)
    [(_ [else e])           `(,e)]
    [(_ [c    t])           `(,(mes:if c t))]
    [(_ [c    t] l ...)     `(,(mes:if c t) ,CNT ,(mes:cond l ...))]))
(define (mes:while c t)     `(,(mes:cmd* #x9E) ,(mes:if c t)))

(define mes:continue        (mes:cmd #x9F))
(define mes:break           (mes:cmd #xA0))
(define mes:menu            (mes:cmd #xA1))
(define mes:mes-jump        (mes:cmd #xA2))
(define mes:mes-call        (mes:cmd #xA3))
(define mes:define-proc     (mes:cmd #xA4))
(define mes:com             (mes:cmd #xA5))
(define mes:wait            (mes:cmd #xA6))
(define mes:window          (mes:cmd #xA7))
(define mes:text-position   (mes:cmd #xA8))
(define mes:text-color      (mes:cmd #xA9))
(define mes:clear           (mes:cmd #xAA))
(define mes:number          (mes:cmd #xAB))
(define mes:call            (mes:cmd #xAC))
(define mes:image           (mes:cmd #xAD))
(define mes:load            (mes:cmd #xAE))
(define mes:execute         (mes:cmd #xAF))
(define mes:recover         (mes:cmd #xB0))
(define mes:set-mem         (mes:cmd #xB1))
(define mes:screen          (mes:cmd #xB2))
(define mes:mes-skip        (mes:cmd #xB3))
(define mes:flag            (mes:cmd #xB4))
(define mes:sound           (mes:cmd #xB6))
(define mes:animate         (mes:cmd #xB7))
(define mes:slot            (mes:cmd #xB8))

(define (mes:proc i) (integer->char (+ i (char->integer PROC))))

(define (mes:cut) `(,CNT))

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
(define (mes:<*> . l) `(,@l))
(define (mes:<.> . l)
  (define (: x)
    (match x
      [`(,a)        `(,(:: a))]
      [`(,a ,r ...) `(,(:: a) ,(mes:cut) ,@(: r))]))
  ;;HACK: handle expr (num) in <.>
  (define (:: x)
    (match x
      [(? number? x) `,(mes:num x)]
      [x             x]))
  (apply mes:<> (: l)))

(define (mes:block? b)
  (match b
   [`(,(? char? a) ,l ... ,(? char? b)) (and (char=? a BEG) (char=? b END))]
   [_                                   #f]))
(define (mes:mes  . l) (flatten `(,l ,END)))
(define (mes:mes* . l) (flatten l))

;; state

(define src '())

(define (mes:init l)
  (set! src l)
  (charset (cfg:charset)))

;; meta

(define (mes:meta . l) '())
(define (mes:engine e) (unless (eq? e 'AI1) (error (format "[AI1] unsupported engine: ~v" e))))
(define (mes:charset f) (charset f))
(define (mes:charset*  k t . l) (apply charset* k t l))
(define (mes:charset** k t . l) (apply charset** k t l))
(define (mes:char-space c) (cfg:char-space c))
(define (mes:fontwidth w) (cfg:fontwidth w))
(define (mes:wordwrap w) (cfg:wordwrap w))

;; extension

(define (mes:include f) (eval `(mes* ,@(file->list f))))

;; compiler-util

(define (char->hex c) (~r (char->integer c) #:base 16 #:min-width 2 #:pad-string "0"))
(define (show-hex l) (string-join (map char->hex (flatten l))))

(provide show-hex)

(provide (filtered-out
          (λ (n) (and (string-prefix? n "mes:") (string-replace n "mes:" "")))
          (all-defined-out)))
