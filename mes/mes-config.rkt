#lang racket/base

(define cfg:dict (make-parameter #x80))
(define cfg:decode (make-parameter #t))
(define cfg:resolve (make-parameter #t))
(define cfg:compress (make-parameter #t))

(provide cfg:dict
         cfg:decode
         cfg:resolve
         cfg:compress)
