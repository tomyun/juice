#lang racket

(require racket/draw)

(require "bmp-common.rkt")
(require "bmp-charset.rkt")

(define w 16)
(define h 16)
(define buf (list->bytes (flatten (make-list (* 16 16) '(#xff #xff #xff #xff)))))

(define (clear k t)
  (define dst (out))
  (define x (* k w))
  (define y (* (+ t 32) h))
  (send dst set-argb-pixels x y w h buf))

(define (clear* k t n)
  (for ([i (range n)])
    (clear (nk k t i)
           (nt t i))))

(provide clear
         clear*)
