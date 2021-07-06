#lang racket/base

(require ansi-color)

(define (display-color   c s) (with-colors c (lambda () (display   s))))
(define (displayln-color c s) (with-colors c (lambda () (displayln s))))

(provide display-color
         displayln-color)
