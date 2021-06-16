#lang racket/base

(define cfg:dict (make-parameter #x80))
(define cfg:protag (make-parameter #f))
(define cfg:resolve (make-parameter #t))
(define cfg:compress (make-parameter #t))

(define (string->protag p)
  (if p
      (let ([n (string->number p)]
            [s (string->symbol p)])
        (if n n s))
      #f))

(provide cfg:dict
         cfg:protag
         cfg:resolve
         cfg:compress
         string->protag)
