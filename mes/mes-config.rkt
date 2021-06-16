#lang racket/base

(define cfg:dict (make-parameter #x80))
(define cfg:resolve (make-parameter #t))
(define cfg:compress (make-parameter #t))

(provide cfg:dict
         cfg:resolve
         cfg:compress)
