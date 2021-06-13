#lang racket/base

(require "mes-compiler.rkt")

(define-namespace-anchor nsa)

(define (compile-mes path)
  (define in (open-input-file path))
  (define src (read in))
  (close-input-port in)
  (define ns (namespace-anchor->namespace nsa))
  (eval (init) ns)
  (define mes (eval src ns))
  (list->bytes (map char->integer mes)))

(provide compile-mes)
