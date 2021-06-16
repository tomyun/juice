#lang racket/base

(define cfg:dict (make-parameter #x80))
(define cfg:protag (make-parameter #f))

(define (string->protag p)
  (if p
      (let ([n (string->number p)]
            [s (string->symbol p)])
        (if n n s))
      #f))

(provide cfg:dict
         cfg:protag
         string->protag)
