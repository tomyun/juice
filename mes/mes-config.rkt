#lang racket/base

(define cfg:dict (make-parameter #x80))
(define cfg:protag (make-parameter #f))

(provide cfg:dict
         cfg:protag)
