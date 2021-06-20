#lang racket/base

(define cfg:charset (make-parameter "pc98"))
(define cfg:dict-base (make-parameter #x80))
(define cfg:decode (make-parameter #t))
(define cfg:resolve (make-parameter #t))
(define cfg:compress (make-parameter #t))

(provide cfg:charset
         cfg:dict-base
         cfg:decode
         cfg:resolve
         cfg:compress)
