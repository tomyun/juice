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

(require bitsyntax)

(require "../../mes-config.rkt")
(require "../../mes-charset.rkt")

;; lexer

(define SPC      #\u20)
(define STR-BEG  #\u21)
(define STR-END  #\u00)
(define ARG      #\u22)

(define NUM0     #\u23)
(define NUM1     #\u28)
(define NUM2     #\u29)

(define CHR-LBEG '(#\u81 #\u6F))
(define CHR-LEND '(#\u81 #\u70))
(define CHR-WAIT '(#\u81 #\u90))
(define CHR-NOP  '(#\u81 #\u97))

(define BEG+     #\uA0)
(define END*     #\uA1)
(define BEG      #\uA2)
(define END      #\uA3)
(define CNT      #\uA4)

(define EOM      '(#\uFF #\uFE))
(define EOS      '(#\uFF #\uFF))

;; compiler

;; reg

(define (mes:reg* n s f)
  (define b
    (let* ([n1 (add1 n)]
           [i  (quotient n1 8)]
           [o  (remainder n1 8)]
           [v  (match f
                 [#f 0]
                 [#t 1]
                 [f  f])])
      (bit-string (0 :: bits 4)
                  (1 :: bits 1)
                  (v :: bits 1)
                  (i :: bits 7)
                  (o :: bits 3))))
  (map integer->char (bytes->list (bit-string->bytes b))))

(define (mes:set-reg n f) (mes:reg* n '= f))

;; var

(define (var->integer c)
  (- (char->integer c) (char->integer #\A)))

(define (mes:var1* c s v)
  (define b
    (let* ([i (var->integer c)]
           [f (match s
                ['+= 3]
                ['-= 1]
                ['=  2])])
      (bit-string (1 :: bits 4)
                  (f :: bits 2)
                  (i :: bits 6)
                  (v :: bits 4))))
  (map integer->char (bytes->list (bit-string->bytes b))))

(define (mes:var2* c s v)
  (define b
    (let* ([i  (var->integer c)]
           [f  (match s
                 ['=  0]
                 ['+= 1]
                 ['-= 2])]
           [m  (if (var? v) 1 0)]
           [j  (if (= m 1) (var->integer v) v)]
           [j1 (arithmetic-shift j -7)]
           [j2 (bitwise-and j #x7F)])
      (bit-string (1  :: bits 4)
                  (f  :: bits 3)
                  (m  :: bits 1)
                  (0  :: bits 2)
                  (i  :: bits 5)
                  (j1 :: bits 1)
                  (0  :: bits 1)
                  (j2 :: bits 7))))
  (map integer->char (bytes->list (bit-string->bytes b))))

(define (mes:var* c s v)
  (if (cfg:extraop)
      (mes:var2* c s v)
      (mes:var1* c s v)))

(define (mes:set-var c v) (mes:var* c '=  v))
(define (mes:inc-var c v) (mes:var* c '+= v))
(define (mes:dec-var c v) (mes:var* c '-= v))

;TODO: macro implementation
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

(define (var? v)
 (match v
  [(? char? c) (char<=? #\A c #\Z)]
  [_           #f]))

;; conds

(define (mes:? . l) l)

(define (mes:= a b)
  (define m (if (var? a) mes:var* mes:reg*))
  (m a '= b))

(define (mes:>= a b)
  (mes:var* a '+= b))

(define (mes:<= a b)
  (mes:var* a '-= b))

;; num

(define (mes:num n)
  (cond
   [(< n 0)         (error (format "negative number not supported: ~a" n))]
   [(<= n 4)        (integer->char (+ n (char->integer NUM0)))] ; 4
   [(<= n #x7F)     `(,NUM1 ,(integer->char n))] ; 127
   [(<= n #xFFFF)   (let* ([n1 (quotient  n #x4000)]
                           [r  (remainder n #x4000)]
                           [n2 (arithmetic-shift r -7)]
                           [n3 (bitwise-and r #x7F)])
                      (map integer->char `(,(+ n1 (char->integer NUM2)) ,n2 ,n3)))] ; 65535
   [else            (error (format "too large number: ~a" n))]))

;; str

;;HACK: counterpart for replacing enhanced (text ...) in translated scripts
;;TODO: implement fuse-str equivalent to fuse-text
(define (mes:str #:col [c #f] . l)
  (define k (if c `(,(mes:text-color c)) '()))
  (define (f t)
    (match t
      [(? string? s) (mes:str* s)]
      [(? symbol? s) (mes:str-func s)]
      [(? number? n) (mes:proc n)]))
  `(,@k ,STR-BEG ,@(map f l) ,STR-END))

(define (mes:str* s)
  (define b (apply bytes-append (map char->sjis-bytes (string->list s))))
  (map integer->char (bytes->list b)))

(define (mes:str-func s)
  (define i (string->number (symbol->string s)))
  (if (number? i)
      (mes:str-raw i)
      (match s
        ['br `(,STR-END ,(mes:text-break) ,STR-BEG)] ;TODO: handle empty STR
        [_   '()])))

(define (mes:str-raw i)
  (match-define `(,c1 ,c2) (integer->sjis i))
  (mes:chr-raw c1 c2))

;; arg

(define (mes:arg s)
  (define b
    (match s
      ;;HACK: avoid char->sjis-bytes due to custom overrides (i.e. space)
      [(? string? s) (apply bytes-append (map char-utf8->bytes-sjis (string->list s)))]
      [(? bytes?  s) s]))
  (define l (map integer->char (bytes->list b)))
  `(,ARG ,@l ,ARG))

;; text

(define (mes:chr-raw c1 c2)
  (cond
     [(and (= #x82 c1) (<= #x9f c2 #xf1)) `(,(integer->char (- c2 #x72)))]
     [else                                (map integer->char `(,c1 ,c2))]))

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

(define (mes:text #:col  [col #f]
                  #:pos  [pos   #f]
                  #:wrap [wrap  #t]
                  . l)
  (define c (if col `(,(mes:text-color col))     '()))
  (define p (if pos `(,(apply mes:text-pos pos)) '()))
  (define w (cond
              [(number? wrap) wrap]
              [wrap           (cfg:wordwrap)]
              [(not wrap)     #f]))
  (define (: t)
    (match t
      [(? string? s) (mes:text* s)]
      [(? symbol? s) (mes:text-func s)]
      [(? number? n) (mes:proc n)]
      ;[(? char?  c) (mes:call c)]
      [t             t]))
  `(,@c ,@p ,@(map : (text-wrap l w))))

(define (mes:text-func s)
  (define i (string->number (symbol->string s)))
  (if (number? i)
      (mes:text-raw i)
      (match s
        ['br (mes:text-break)]
        [_   '()])))

(define (mes:text* s)
  (define l
    (with-handlers ([exn:fail? (λ (v) (error (format "(text ~v) got ~a" s (exn-message v))))])
      (flatten (map char->sjis (string->list s)))))
  (define (: l)
    (match l
      [`(,c1 ,c2 ,r ...) `(,@(mes:chr-raw c1 c2) ,@(: r))]
      [x                 x]))
  (: l))

(define (mes:text-raw . l)
  (define (f i)
    (match-define `(,c1 ,c2) (integer->sjis i))
    (mes:chr-raw c1 c2))
  (flatten (map f l)))

;; special chr

(define (mes:loop . l) `(,CHR-LBEG ,@l ,CHR-LEND))
(define (mes:wait$) CHR-WAIT)
(define (mes:nop@)  CHR-NOP)

;; command

(define (mes:cmd i) (λ p `(,(integer->char i) ,@(mes:params p))))

(define mes:text-break    (mes:cmd #xA5))
(define mes:text-frame    (mes:cmd #xA6))
(define mes:text-pos      (mes:cmd #xA7))
(define mes:text-color    (mes:cmd #xA8))
(define mes:text-delay    (mes:cmd #xA9))
(define mes:text-reset    (mes:cmd #xAA))
(define mes:wait          (mes:cmd #xAB))
(define mes:delay         (mes:cmd #xAC))
(define mes:menu1         (mes:cmd #xAD))
(define mes:menu2         (mes:cmd #xAE))
(define mes:seg-call      (mes:cmd #xAF))
(define mes:exec-file     (mes:cmd #xB0))
(define mes:mes-jump      (mes:cmd #xB1))
(define mes:branch-random (mes:cmd #xB2))
(define mes:branch-index  (mes:cmd #xB3))
(define mes:branch-var    (mes:cmd #xB4))
(define mes:branch-reg    (mes:cmd #xB5))
;(define mes:mouse1?       (mes:cmd #xB6))
(define mes:mouse         (mes:cmd #xB7))
;(define mes:?             (mes:cmd #xB8))
(define mes:define-proc   (mes:cmd #xB9))
(define mes:proc          (mes:cmd #xBA))
(define mes:repeat        (mes:cmd #xBB))
(define mes:if            (mes:cmd #xBC))
(define mes:when          (mes:cmd #xBD))
(define mes:flag-save     (mes:cmd #xBE))
(define mes:flag-load     (mes:cmd #xBF))
(define mes:mes-load?     (mes:cmd #xC0))
;(define mes:?             (mes:cmd #xC1))
;(define mes:?             (mes:cmd #xC2))
;(define mes:?             (mes:cmd #xC3))
;(define mes:?             (mes:cmd #xC4))
;(define mes:?             (mes:cmd #xC5))
;(define mes:?             (mes:cmd #xC6))
;(define mes:?             (mes:cmd #xC7))
(define mes:load-mem      (mes:cmd #xC8))
(define mes:image-file    (mes:cmd #xC9))
;(define mes:?             (mes:cmd #xCA))
;(define mes:text-skip-delay? (mes:cmd #xCB))
;(define mes:?             (mes:cmd #xCC))
(define mes:exec-mem      (mes:cmd #xCD))
;(define mes:?             (mes:cmd #xCE))
(define mes:image-mem     (mes:cmd #xCF))
(define mes:sound         (mes:cmd #xD0))
;(define mes:?             (mes:cmd #xD1))
;(define mes:?             (mes:cmd #xD2))
;(define mes:?             (mes:cmd #xD3))
;(define mes:?             (mes:cmd #xD4))
(define mes:decrypt       (mes:cmd #xD5))
;(define mes:nop           (mes:cmd #xD6))
;(define mes:nop           (mes:cmd #xD7))
;(define mes:nop           (mes:cmd #xD8))

;; param

(define (mes:sym s)
  (string->list (symbol->string s)))

(define (mes:param p)
  (match p
    [(? block?  b) b]
    [(? string? s) (mes:arg s)]
    [(? bytes?  b) (mes:arg b)]
    [(? number? n) (mes:num n)]
    [(? symbol? s) (mes:sym s)]
    [p             p]))
(define (mes:params . p)
  (define (: x)
    (match x
      [`(()       (,r ... ,a)) (: `((,(mes:param a))     ,r))]
      [`((,l ...) (,r ... ,a)) (: `((,(mes:param a) ,@l) ,r))]
      [`((,l ...) ())          l]))
  (: `(() ,@p)))

;; block

(define (mes:<>  . l) `(,BEG ,@l ,END))

(define (mes:<*> . l) `(,BEG ,@l, END))
(define (mes:<+> . l)
  (define (: x)
    (match x
      ['()          '()]
      [`(,a)        `(,a)]
      [`(,a ,r ...) `(,a ,CNT ,@(: r))]
      [x            x]))
  `(,BEG+ ,(: l)))

(define (mes:* . l) `(,@l ,END*))
(define (mes:+ . l) `(,l))

(define (mes:</> . l)
  (define (: x)
    (match x
      ['()          '()]
      [`(,a)        `(,a)]
      [`(,a ,r ...) `(,a ,CNT ,@(: r))]
      [x            x]))
  `(,BEG ,(: l) ,END))

(define (mes:/  . l) l)
(define (mes:// . l) l)

(define (block? b)
  (match b
   [`(,(? char? a) ,l ... ,(? char? b)) (and (char=? a BEG) (char=? b END))]
   [_                                   #f]))

(define (mes:seg  . l) (flatten `(,l ,EOS)))
(define (mes:seg* . l) (flatten `(,l ,EOS)))

(define (mes:mes  . l) (flatten `(,l ,EOM)))

;; state

(define src '())

(define (mes:init l)
  (set! src l)
  (charset (cfg:charset)))

;; meta

(define (mes:meta . l)          '())
(define (mes:engine e)          (unless (eq? e 'ADV) (error (format "[Adv] unsupported engine: ~v" e))))
(define (mes:charset f)         (charset f))
(define (mes:charset*  k t . l) (apply charset* k t l))
(define (mes:charset** k t . l) (apply charset** k t l))
(define (mes:char-space c)      (cfg:char-space c))
(define (mes:fontwidth w)       (cfg:fontwidth w))
(define (mes:extraop x)         (cfg:extraop x))
(define (mes:wordwrap w)        (cfg:wordwrap w))

;; extension

(define (mes:include f) (eval `(mes* ,@(file->list f))))

;; compiler-util

(define (char->hex c) (~r (char->integer c) #:base 16 #:min-width 2 #:pad-string "0"))
(define (show-hex l)  (string-join (map char->hex (flatten l))))

(provide show-hex)

;; compatibility

;(include "mes-compiler-compat.rkt")

(provide (filtered-out
          (λ (n) (and (string-prefix? n "mes:") (string-replace n "mes:" "")))
          (all-defined-out)))
