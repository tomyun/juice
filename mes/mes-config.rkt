#lang racket/base

(require racket/match)
(require racket/set)
(require racket/string)

;; config

(define cfg:engine    (make-parameter 'AI5))
(define cfg:charset   (make-parameter "pc98"))
(define cfg:charspc   (make-parameter #\u3000))
(define cfg:fontwidth (make-parameter 2))
(define cfg:dictbase  (make-parameter #x80))
(define cfg:extraop   (make-parameter #f))
(define cfg:decode    (make-parameter #t))
(define cfg:resolve   (make-parameter #t))
(define cfg:protag    (make-parameter #f))
(define cfg:wordwrap  (make-parameter #f))
(define cfg:compress  (make-parameter #t))

(provide cfg:engine
         cfg:charset
         cfg:charspc
         cfg:fontwidth
         cfg:dictbase
         cfg:extraop
         cfg:decode
         cfg:resolve
         cfg:protag
         cfg:wordwrap
         cfg:compress)

;; engine

(define (set-engine e)
  (cfg:engine
    (match e
      ['AI2    'AI1]
      ['AI4    'AI5]
      ['AI5X   (cfg:dictbase #xD0)
               (cfg:extraop  #t)
               'AI5]
      [e       e])))

;; protag

(define (set-protag p)
  (define (split)
    (define l (map string-trim (string-split p ",")))
    (define (: s)
      (match s
        [(regexp #rx"[0-9]+") (string->number s)]
        [(regexp #rx"[A-Z]")  (string-ref s 0)]
        [s                    (error (format "unsupported protag proc/call: ~a" s))]))
    (map : l))
  (cfg:protag
    (match p
      ["all"  #t]
      ["none" #f]
      [s      (split)])))

(define (protag? x)
  (define p (cfg:protag))
  (match p
    [#t #t]
    [#f #f]
    [p  (set-member? p x)]))

(provide set-engine
         set-protag
         protag?)
