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
  (match-define `(MES ,code) f)
  (define r (parse-result (parser) code))
  (fuse (resolve (lower r))))

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
  (define c (sjis->char j))
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
      [`(,x ...)     (map : x)]
      [x             x]))
  (: l))

(define (resolve-exp c)
  (match (char->integer c)
    [#x21 '!=]
    [#x23 '~b]
    [#x25 '%]
    [#x26 '&&]
    [#x2A '*]
    [#x2B '+]
    [#x2D '-]
    [#x2F '/]
    [#x3C '<]
    [#x3D '==]
    [#x3E '>]
    [#x3F '?]
    [#x5C '~]
    [#x5E '^]
    [#x7C '//]))

(define (resolve-cmd c)
  (define i (char->integer c))
  (define r (unknown-op-name 'cmd i))
  (if (cfg:resolve)
    (match i
      [#x99 'set-reg:]
      [#x9A 'set-var]
      [#x9B 'set-arr~]
      [#x9C 'set-arr~b]
      [#x9E 'while]
      [#x9F 'continue]
      [#xA0 'break]
      [#xA1 'select]
      [#xA2 'mes-jump]
      [#xA3 'mes-call]
      [#xA4 'define-proc]
      [#xA5 'com]
      [#xA6 'wait]
      [#xA7 'window]
      [#xA8 'locate]
      [#xA9 'text-color]
      [#xAA 'clear]
      [#xAB 'number]
      [#xAC 'call]
      [#xAD 'pic]
      [#xAE 'load-file]
      [#xAF 'input]
      [#xB0 'recover]
      [#xB1 'set]
      [#xB2 'screen]
      [#xB4 'flag]
      [#xB5 'text-position]
      [#xB6 'play]
      [#xB7 'bg]
      [#xB8 'map-init]
      [#xB9 'set-bg]
      [#xBA 'map-put]
      [#xBB 'chara-put]
      [#xBC 'window-put]
      [_    r])
    r))

(define (unknown-op-name s i) `(,s ,i))

(define (fuse l)
  (define f
    `(,fuse-while
      ,fuse-operator
      ,fuse-text
      ,fuse-text-color
      ,fuse-text-proc-call
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

(define (fuse-meta l)
  (define m
    `((engine   ',(cfg:engine))
      (charset  ,(cfg:charset))))
  (match l
    [`(mes ,r ...) `(mes (meta ,@m) ,@r)]))

(provide load-mes
         load-mes-snippet)
