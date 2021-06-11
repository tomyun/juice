#lang racket/base

(require "mes-compiler.rkt")

(define-namespace-anchor nsa)

(define (compile-mes path)
  (define src (read (open-input-file path)))
  (define mes (eval src (namespace-anchor->namespace nsa)))
  (list->bytes (map char->integer mes)))

(provide compile-mes)
