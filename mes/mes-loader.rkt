#lang racket/base

(require racket/format)
(require racket/function)
(require racket/match)

(require "mes-config.rkt")
(require "mes-opener.rkt")
(require "mes-parsack.rkt")

(define (load-mes path)
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

(define (fold-expr l)
  (define (: x)
    (match x
      [`((,a ,b ,s ...)      (term2 ,f) ,r ...) (: (cons (cons `((exp ,f) ,b ,a) s) r))]
      [`((,a ,s ...)         (term1 ,f) ,r ...) (: (cons (cons `((exp ,f) ,a) s) r))]
      [`((,s ...)            (term0 ,f) ,a ,r ...) (: (cons (cons `((exp ,f) ,a) s) r))]
      [`((,s ...) ,a ,r ...) (: (cons (cons a s) r))]
      [`((,s))               s]
      [`(())                 `(_)])) ; empty expr in np2/MISATO.MES
  (: (cons '() l)))

(define (resolve l)
  (define (: x)
    (match x
      [`(exp ,x) (resolve-exp x)]
      [`(cmd ,x) (resolve-cmd x)]
      [`(sys ,c ...) (apply resolve-sys c)]
      [`(,x ...) (map : x)]
      [x x]))
  (: l))

(define (resolve-exp c)
  (match (char->integer c)
    [#x20 '+]
    [#x21 '-]
    [#x22 '*]
    [#x23 '/]
    [#x24 '%]
    [#x25 '!] ;FIXME: use single `|` in own #lang?
    [#x26 '&]
    [#x27 '==]
    [#x28 '!=]
    [#x29 '>]
    [#x2A '<]
    [#x2B '~]
    [#x2C '~b]
    [#x2D ':]
    [#x2E ':]
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
      [#x15 'print-number]
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
      ,fuse-logic
      ,(curry fuse-dic dict)
      ,(curry fuse-dic-header dict)
      ,fuse-text
      ,fuse-protag))
  ((apply compose1 (reverse f)) l))

(define (fuse-while l)
  (define (: x)
    (match x
      [`((while) (if ,c ,t) ,r ...) (cons `(while ,(: c) ,(: t)) (: r))]
      [`(,a ,r ...)                 (cons (: a) (: r))]
      [a                            a]))
  (: l))

(define (fuse-logic l)
  (define (: x)
    (match x
      [`(& (& ,a ,b) ,c) `(& ,@(:& a) ,@(:& b) ,@(:& c))]
      [`(! (! ,a ,b) ,c) `(! ,@(:! a) ,@(:! b) ,@(:! c))]
      [`(,a ...)         (map : a)]
      [a                 a]))
  (define (:& x)
    (match x
      [`(& ,a ,b) `(,@(:& a) ,@(:& b))]
      [e          `(,e)]))
  (define (:! x)
    (match x
      [`(! ,a ,b) `(,@(:! a) ,@(:! b))]
      [e          `(,e)]))
  (: l))

(define (fuse-dic dict l)
  (define n (length dict))
  (define (: x)
    (match x
      [`(dic ,i) (if (< i n)
                     (match (list-ref dict i)
                       [(? char? c)  `(chr ,c)]
                       [`'(,c1 ,c2)   `(chr-raw ,c1 ,c2)])
                     `(dic ,i))]
      [`(,a ...) (map : a)]
      [a         a]))
  (: l))

(define (fuse-dic-header dict l)
  (match l
   [`(mes ,r ...) `(mes (dict ,@dict) ,@r)]))

(define (fuse-text l)
  (define (: x)
    (match x
      [`((chrs ,c ...) ,r ...) (append (:: c) (: r))]
      [`(,a ,r ...)            (cons (: a) (: r))]
      [a                       a]))
  (define (:: x)
     (match x
       [`((chr ,c) ..1 ,r ...)     (cons `(text ,(apply string c)) (:: r))]
       [`((chr-raw ,c ...) ,r ...) (cons `(chr-raw ,@c) (:: r))]
       [a                          a]))
  (: l))

(define (fuse-protag l)
  (define p (cfg:protag))
  (define f
    (match p
      [(? number?) `(proc ,p)] ; 0: nanpa1 & etc, 3: kakyu
      [(? symbol?) `(call ,p)] ; Z: elle
      [_           p]))
  (define (: x)
      (match x
        [`((text ,t1 ...) ,(== f) (text ,t2 ...) ,r ...) (: (cons `(text ,@t1 ,p ,@t2) r))]
        [`(,a ,r ...)                                    (cons (: a) (: r))]
        [a                                               a]))
  (: l))

(provide load-mes
         load-mes-snippet)
