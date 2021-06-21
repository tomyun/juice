#lang racket/base

(define cfg:charset (make-parameter "pc98"))
(define cfg:dictbase (make-parameter #x80))
(define cfg:extraop (make-parameter #f))
(define cfg:decode (make-parameter #t))
(define cfg:resolve (make-parameter #t))
(define cfg:protag (make-parameter #t))
(define cfg:wordwrap (make-parameter #f))
(define cfg:compress (make-parameter #t))

(provide cfg:charset
         cfg:dictbase
         cfg:extraop
         cfg:decode
         cfg:resolve
         cfg:protag
         cfg:wordwrap
         cfg:compress)
