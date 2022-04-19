#lang racket/base

(require racket/format)
(require racket/function)
(require racket/list)
(require racket/match)
(require racket/string)

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

;; lower

(define (lower l)
  (define (: x)
    (match x
      [`(num ,n)              n]
      [`(var ,v)              (lower-var v)]
      [`(chr-ascii  ,c)       (lower-chr-ascii  c)]
      [`(chr-sjis1  ,i)       (lower-chr-sjis1  i)]
      [`(chr-sjis2  ,j ...)   (lower-chr-sjis2  j)]
      [`(chr-sjis2+ ,j ...)   (lower-chr-sjis2+ j)]
      [`(,a (?) ,b ...)       `(,(: a) ,@(: b))]
      [`(,c (params ,p ...))  `(,(: c) ,@(: p))]
      [`(param ,p)            (: p)]
      [`(,a ...)              (map : a)]
      [x                      x]))
  (: l))

(define (lower-var v)
  (string->symbol (string v)))

(define (lower-chr-ascii c)
  ;;HACK: handle modified ASCII characters
  (match c
    [#\\ #|5C|# `(chr #\¥)]
    [#\~ #|7E|# `(chr #\‾)]
    [c          `(chr ,c)]))

(define (lower-chr-sjis1 i)
  `(chr ,(sjis->char `(,i 0))))

(define (lower-chr-sjis2 j)
  (define c (sjis->char j))
  (if (and c (cfg:decode))
    `(chr     ,c)
    `(chr-raw ,@j)))

(define (lower-chr-sjis2+ j)
  `(chr-raw ,@j))

(define (lower-chrs! c)
  `(str ,(list->string c)))

;; resolve

(define (resolve l)
  (define (: x)
    (match x
      ;[`(chr, c) (resolve-chr c)]
      [`(cmd ,c) (resolve-cmd c)]
      [`(,a ...) (map : a)]
      [x         x]))
  (: l))

(define (resolve-chr c)
  (match c
    ;;TODO: newline, continue, break?
    [#\｛ '(loop-begin)]
    [#\｝ '(loop-end)]
    [#\＄ '(wait$)]
    [#\＠ '(nop@)]
    [c    c]))

(define (resolve-cmd c)
  (define i (char->integer c))
  (define r (unknown-op-name 'cmd i))
  (if (cfg:resolve)
    (match i
      [#xA5 'text-break]
      [#xA6 'text-frame]
      [#xA7 'text-pos]
      [#xA8 'text-color]
      [#xA9 'text-delay]
      [#xAA 'text-reset]
      [#xAB 'wait]
      [#xAC 'delay]
      [#xAD 'menu1]
      [#xAE 'menu2]
      [#xAF 'seg-call]
      [#xB0 'exec-file]
      [#xB1 'mes-jump]
      [#xB2 'branch-random]
      [#xB3 'branch-index]
      [#xB4 'branch-var]
      [#xB5 'branch-reg]
      ;[#xB6 'mouse1?]
      [#xB7 'mouse]
      ;[#xB8 '?]
      [#xB9 'define-proc]
      [#xBA 'proc]
      [#xBB 'repeat]
      [#xBC 'if]
      [#xBD 'when]
      [#xBE 'flag-save]
      [#xBF 'flag-load]
      [#xC0 'mes-load?]
      ;[#xC1 '?]
      ;[#xC2 '?]
      ;[#xC3 '?]
      ;[#xC4 '?]
      ;[#xC5 '?]
      ;[#xC6 '?]
      ;[#xC7 '?]
      [#xC8 'load-mem]
      [#xC9 'image-file]
      ;[#xCA '?]
      ;[#xCB 'text-skip-delay?]
      ;[#xCC '?]
      [#xCD 'exec-mem]
      ;[#xCE '?]
      [#xCF 'image-mem]
      [#xD0 'sound]
      ;[#xD1 '?]
      ;[#xD2 '?]
      ;[#xD3 '?]
      ;[#xD4 '?]
      [#xD5 'decrypt]
      [#xD6 (unknown-op-name 'nop i)]
      [#xD7 (unknown-op-name 'nop i)]
      [#xD8 (unknown-op-name 'nop i)]
      ;[#xD9 '?]
      ;[#xDA '?]
      ;[#xDB '?]
      ;[#xDC '?]
      ;[#xDD '?]
      ;[#xDE '?]
      ;[#xDF '?]
      [_    r])
    r))

(define (unknown-op-name s i) `(,s ,i))

;; fuse

(define (fuse l)
  (define f
    `(;,fuse-seg
      ,fuse-arg
      ,fuse-str
      ,fuse-text
      ,fuse-text-break
      ,fuse-text-proc
      ,fuse-text-raw
      ;,fuse-text-multiple
      ,fuse-text-pos
      ,fuse-text-color
      ,fuse-meta))
  ((apply compose1 (reverse f)) l))

;;TODO: remove
(define (fuse-seg l)
  (define (: x)
    (match x
      [`((seg (?) ,s ...) ,r ...)  `((seg ,@s) ,@(: r))]
      [`(,a               ,r ...)  `(,(: a)    ,@(: r))]
      [x                           x]))
  (: l))

(define (fuse-arg l)
  (define (: x)
    (match x
      [`((arg ,c ...) ,r ...) `(,(:: c) ,@(: r))]
      [`(,a           ,r ...) `(,(: a)  ,@(: r))]
      [x                      x]))
  (define (:: x)
    (define t (remove-duplicates (map car x)))
    (if (not (member 'chr-byte t))
        (list->string (map cadr x))
        (list->bytes  (map char->integer (map cadr (f x))))))
  (define (f x)
    (define (g c)
      (define b (char->sjis-bytes c))
      (map (λ (i) `(chr-byte ,(integer->char i))) (bytes->list b)))
    (match x
      [`((chr ,c) ,r ...) `(,@(g c) ,@(f r))]
      [`(,a       ,r ...) `(,(f a)  ,@(f r))]
      [x                  x]))
  (: l))

(define (integer->symbol i)
  (string->symbol (number->string i)))

(define (fuse-str l)
  (define (: x)
    (match x
      [`((chrs! ,c ...) ,r ...) `((str ,@(:: c)) ,@(: r))]
      [`(,a             ,r ...) `(,(: a)         ,@(: r))]
      [x                        x]))
  (define (:: x)
    ;;TODO: check chr?
    ;`(str ,(apply string (map cadr x))))
    (match x
      [`((chr ,c) ..1     ,r ...) `(,(apply string c)                          ,@(:: r))]
      [`((chr-raw ,n ...) ,r ...) `(',(integer->symbol (sjis->integer-sjis n)) ,@(:: r))]
      [x                          x]))
  (: l))

(define (fuse-text l)
  (define (: x)
    (match x
      [`((chrs ,c ...) ,r ...) `(,@(:: c) ,@(: r))]
      [`(,a            ,r ...) `(,(: a)   ,@(: r))]
      [x                       x]))
  (define (:: x)
    (match x
      [`((chr ,c) ..1         ,r ...) `(,@(:/ (apply string c))                 ,@(:: r))]
      [`((chr-raw ,n ...) ..1 ,r ...) `((text-raw ,@(map sjis->integer-sjis n)) ,@(:: r))]
      [x                              x]))
  ;;TODO: perhaps not needed for Adv?
  (define (:/ s)
    (define r (regexp-match-positions* #rx"\n+|$" s))
    (define n (remove-duplicates (map cdr r)))
    (for/list ([i `(0 ,@(drop-right n 1))]
               [j n])
      `(text ,(substring s i j))))
  (: l))

(define (fuse-text-break l)
  (define (: x)
    (match x
      [`((text-break)               ,r ...) `((text 'br)     ,@(: r))]
      [`((text ,t ...) (text-break) ,r ...) `((text ,@t 'br) ,@(: r))]
      [`(,a                         ,r ...) `(,(: a)         ,@(: r))]
      [x                                    x]))
  (: l))

(define (fuse-text-proc l)
  (define (: x)
    (match x
      [`((proc ,(? number? p)) ,r ...) #:when (protag? p) `((text ,p)        ,@(: r))]
      [`((proc ,p)             ,r ...) #:when (protag? p) `((text (proc ,p)) ,@(: r))]
      [`(,a                    ,r ...)                    `(,(: a)           ,@(: r))]
      [x                                                  x]))
  (: l))

(define (fuse-text-raw l)
  (define (: x)
    (match x
      [`((text-raw ,i) (text ,t ...) ,r ...) (: `((text ',(integer->symbol i) ,@t) ,@r))]
      [`((text ,t ...) (text-raw ,i) ,r ...) (: `((text ,@t ',(integer->symbol i)) ,@r))]
      [`(,a                          ,r ...) `(,(: a)                              ,@(: r))]
      [x                                     x]))
  (: l))

(define (fuse-text-multiple l)
  (define (: x)
    (match x
      [`((text ,t1 ... ,t) (text ,t2 ...) ,r ...) (: `((text ,@t1 ,t ,@t2) ,@r))]
      [`(,a                               ,r ...) `(,(: a) ,@(: r))]
      [x                                          x]))
  (: l))

(define (fuse-text-pos l)
  (define (k p) `(,(string->keyword "pos") ',p))
  (define (: x)
    (match x
      [`((text-pos ,p ...) (text ,t ...) ,r ...) (: `((text ,@(k p) ,@t) ,@r))]
      [`(,a                              ,r ...) `(,(: a)             ,@(: r))]
      [x                                         x]))
  (: l))

(define (fuse-text-color l)
  (define (k c) `(,(string->keyword "col") ,c))
  (define (: x)
    (match x
      [`((text-color ,c) (text ,t ...) ,r ...) (: `((text ,@(k c) ,@t) ,@r))]
      [`(,a                            ,r ...) `(,(: a)             ,@(: r))]
      [x                                       x]))
  (: l))

(define (fuse-meta l)
  (define m
    (remove (void) `((engine   ',(cfg:engine))
                     (charset  ,(cfg:charset))
                     ,(when (cfg:extraop) `(extraop ,(cfg:extraop))))))
  (match l
    [`(mes ,r ...) `(mes (meta ,@m) ,@r)]))

(provide load-mes
         load-mes-snippet)
