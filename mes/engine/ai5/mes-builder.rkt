#lang racket/base

(require racket/file)

(require "mes-compiler.rkt")

(define-namespace-anchor nsa)

(define (build-mes path)
  (define src (file->value path))
  (define ns (namespace-anchor->namespace nsa))
  (eval `(init ',src) ns)
  (define mes (eval src ns))
  (list->bytes (map char->integer mes)))

(provide build-mes)
