#lang racket/base

(require racket/format)
(require racket/function)
(require racket/match)

(require "../../mes-config.rkt")
(require "../../mes-charset.rkt")
(require "mes-opener.rkt")
(require "mes-parser.rkt")

(define (load-mes path)
  (charset (cfg:charset))
  (define f (open-mes path))
  (match-define `(MES ,dict ,code) f)
  (define r (parse-result (parser) code))
  (fuse (resolve (lower r)) dict))

(define (load-mes-snippet h [p p:<mes>])
  (parse-result (parser p) (open-mes-snippet h)))

(define (lower l)
  (define (: x)
    (match x
      [`(num ,n)                n]
      [`(var ,v)                (lower-var v)]
      [`(chr-raw ,j ...)        (lower-chr j)]
      [`(exprs ,e ...)          (: e)]
      [`(,c ... (exprs ,e ...)) `(,@(: c) ,@(: e))]
      [`(expr ,e ...)           (fold-expr (: e))]
      [`(,c (params ,p ...))    `(,(: c) ,@(: p))]
      [`(param ,p)              (: p)]
      ;[`(<> ,s ...)           `(,@(: s))]
      [`(<>* ,s)                (: s)]
      [`(,a ...)                (map : a)]
      [a                        a]))
  (: l))

(define (lower-var v)
  (match v
    [(? char? c) (string->symbol (string v))]
    [a           a]))

(define (lower-chr j)
  (define c
    (match (sjis->char j)
      [c #:when (eq? c (cfg:char-newline)) #\newline]
      [c                                   c]))
  (if (and c (cfg:decode))
    `(chr     ,c)
    `(chr-raw ,@j)))

(define (fold-expr l)
  (define (: x)
    (match x
      [`((,a ,b ,s ...)      (term2 ,f) ,r ...)    (: `((((exp ,f) ,b ,a) ,@s) ,@r))]
      [`((,a ,s ...)         (term1 ,f) ,r ...)    (: `((((exp ,f) ,a)    ,@s) ,@r))]
      [`((,s ...)            (term0 ,f) ,a ,r ...) (: `((((exp ,f) ,a)    ,@s) ,@r))]
      [`((,s ...) ,a ,r ...)                       (: `((,a               ,@s) ,@r))]
      [`((,s))                                     s]
      [`(())                                       `(_)])) ; empty expr in np2/MISATO.MES
  (: `(() ,@l)))

(define (resolve l)
  (define (: x)
    (match x
      [`(exp ,x)     (resolve-exp x)]
      [`(cmd ,x)     (resolve-cmd x)]
      [`(sys ,c ...) (apply resolve-sys c)]
      [`(,x ...)     (map : x)]
      [x             x]))
  (: l))

(define (resolve-exp c)
  (match (char->integer c)
    [#x20 '+]
    [#x21 '-]
    [#x22 '*]
    [#x23 '/]
    [#x24 '%]
    [#x25 '//]
    [#x26 '&&]
    [#x27 '==]
    [#x28 '!=]
    [#x29 '>]
    [#x2A '<]
    [#x2B '~]
    [#x2C '~b]
    [#x2D ':]
    [#x2E '::]
    [#x2F '?]))

(define (resolve-cmd c)
  (define i (char->integer c))
  (define r (unknown-op-name 'cmd i))
  (if (cfg:resolve)
    (match i
      [#x10 'text-color]
      [#x11 'wait]
      [#x12 'define-proc]
      [#x13 'proc]
      [#x14 'call]
      [#x15 'number]
      [#x16 'delay]
      [#x17 'clear]
      [#x18 'color]
      [#x19 'util]
      [#x1A 'animate]
      [_    r])
    r))

(define (resolve-sys c [n #f])
  (define i (char->integer c))
  (define r (unknown-op-name 'sys i))
  (define s
    (if (cfg:resolve)
      (match i
        [#x10 'while]
        [#x11 'continue]
        [#x12 'break]
        [#x13 'menu-show]
        [#x14 'menu-init]
        [#x15 'mouse]
        [#x16 'palette]
        [#x17 'box]
        [#x18 'box-inv]
        [#x19 'blit]
        [#x1A 'blit-swap]
        [#x1B 'blit-mask]
        [#x1C 'load-file]
        [#x1D 'load-image]
        [#x1E 'mes-jump]
        [#x1F 'mes-call]
        [#x21 'flag]
        [#x22 'slot]
        [#x23 'click]
        [#x24 'sound]
        [#x26 'field]
        [_    r])
      r))
  (cond [n    (match s [`(,l ...) `(,@l ,n)]
                       [s         `(,s ,n)])]
        [else                     s]))

(define (unknown-op-name s i) `(,s ,i))

(define (fuse l dict)
  (define f
    `(,fuse-while
      ,fuse-operator
      ,(curry fuse-dic dict)
      ,fuse-text
      ,fuse-text-color
      ,fuse-text-proc-call
      ,(curry fuse-dict dict)
      ,fuse-meta))
  ((apply compose1 (reverse f)) l))

(define (fuse-while l)
  (define (: x)
    (match x
      [`((while) (if ,c ,t) ,r ...) `((while ,(: c) ,(: t)) ,@(: r))]
      [`(,a ,r ...)                 `(,(: a)                ,@(: r))]
      [a                            a]))
  (: l))

(define (fuse-operator l)
  (define (: x)
    (match x
      [`(&& (&& ,a ,b) ,c) `(&& ,@(:&& a) ,(: b) ,(: c))]
      [`(// (// ,a ,b) ,c) `(// ,@(:// a) ,(: b) ,(: c))]
      [`(+  (+  ,a ,b) ,c) `(+  ,@(:+  a) ,(: b) ,(: c))]
      [`(-  (-  ,a ,b) ,c) `(-  ,@(:-  a) ,(: b) ,(: c))]
      [`(*  (*  ,a ,b) ,c) `(*  ,@(:*  a) ,(: b) ,(: c))]
      [`(/  (/  ,a ,b) ,c) `(/  ,@(:/  a) ,(: b) ,(: c))]
      [`(,a ...)           (map : a)]
      [a                   a]))
  (define (:&& x)
    (match x
      [`(&& ,a ,b) `(,@(:&& a) ,@(:&& b))]
      [e           `(,e)]))
  (define (:// x)
    (match x
      [`(// ,a ,b) `(,@(:// a) ,@(:// b))]
      [e           `(,e)]))
  (define (:+ x)
    (match x
      [`(+  ,a ,b) `(,@(:+  a) ,@(:+  b))]
      [e           `(,e)]))
  (define (:- x)
    (match x
      [`(-  ,a ,b) `(,@(:-  a) ,@(:-  b))]
      [e           `(,e)]))
  (define (:* x)
    (match x
      [`(*  ,a ,b) `(,@(:*  a) ,@(:*  b))]
      [e           `(,e)]))
  (define (:/ x)
    (match x
      [`(/  ,a ,b) `(,@(:/  a) ,@(:/  b))]
      [e           `(,e)]))
  (: l))

(define (fuse-dic dict l)
  (define n (length dict))
  (define (f i)
    (if (< i n)
      (list-ref dict i)
      (error (format "dict index ~a >= dict size ~a; try `--dictbase D0`" i n))))
  (define (: x)
    (match x
      [`(dic ,i) (lower-chr (f i))]
      [`(,a ...) (map : a)]
      [a         a]))
  (: l))

(define (fuse-text l)
  (define (: x)
    (match x
      [`((chrs ,c ...) ,r ...) `(,@(:: c) ,@(: r))]
      [`(,a ,r ...)            `(,(: a)   ,@(: r))]
      [a                       a]))
  (define (:: x)
     (match x
       [`((chr ,c) ..1 ,r ...)         `((text     ,(apply string c))       ,@(:: r))]
       [`((chr-raw ,n ...) ..1 ,r ...) `((text-raw ,@(map sjis->integer n)) ,@(:: r))]
       [a                              a]))
  (: l))

(define (fuse-text-color l)
  (define (f n) `(,(string->keyword "color") ,n))
  (define (: x)
    (match x
      [`((text-color ,c) (text ,t ...) ,r ...) `((text ,@(f c) ,@t) ,@(: r))]
      [`(,a ,r ...)                            `(,(: a)             ,@(: r))]
      [a                                       a]))
  (: l))

(define (fuse-text-proc-call l)
  (define (: x)
    (match x
      [`((text ,t1 ...) (,(or 'proc 'call) ,p) (text ,t2 ...) ,r ...)
       #:when (protag? p)                                             (: `((text ,@t1 ,p ,@t2) ,@r))]
      [`(,a ,r ...)                                                   `(,(: a) ,@(: r))]
      [a                                                              a]))
  (: l))

(define (fuse-dict dict l)
  (define (f j)
    (define c (sjis->char j))
    (if (and c (cfg:decode)) c `',j))
  (match l
    [`(mes ,r ...) `(mes (dict ,@(map f dict)) ,@r)]))

(define (fuse-meta l)
  (define m
    `((engine   ',(cfg:engine))
      (charset  ,(cfg:charset))
      (dictbase ,(cfg:dictbase))
      (extraop  ,(cfg:extraop))))
  (match l
    [`(mes ,r ...) `(mes (meta ,@m) ,@r)]))

(provide load-mes
         load-mes-snippet)
