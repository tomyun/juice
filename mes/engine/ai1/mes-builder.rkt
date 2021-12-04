#lang racket/base

(require racket/file)
(require racket/list)

(require "mes-compiler.rkt")

(define-namespace-anchor nsa)

(define (build-mes path)
  (define src (file->value path))
  (build-mes-source src))

(define (build-mes-source src)
  (define ns (namespace-anchor->namespace nsa))
  (eval `(init ',src) ns)
  (define mes (eval src ns))
  (list->bytes (map char->integer (flatten mes))))

(provide build-mes
         build-mes-source)
