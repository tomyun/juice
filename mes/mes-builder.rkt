#lang racket/base

(require "mes-compiler.rkt")

(define-namespace-anchor nsa)

(define (compile-mes path)
  (define in (open-input-file path))
  (define src (read in))
  (close-input-port in)
  (define mes (eval src (namespace-anchor->namespace nsa)))
  (list->bytes (map char->integer mes)))

(provide compile-mes)
